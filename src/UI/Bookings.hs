{-# LANGUAGE OverloadedStrings #-}
module UI.Bookings (runBookingsUI) where

import Data.Monoid
import Lens.Micro ((^.))
import qualified Brick.Main as M
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import Brick.Widgets.Core
import Brick.Util (fg, on)
import Brick
import qualified Brick.BChan
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Graphics.Vty
import qualified Graphics.Vty.Config
import qualified Control.Concurrent
import Control.Monad.IO.Class

import qualified CE.Client as CE

data State = State CE.Site (L.List () CE.Booking)

data Event = RefreshBookingsEvent

initialState :: CE.Site -> [CE.Booking] -> State
initialState site bookings = State site (L.list () (Vec.fromList bookings) 3)

drawUI :: State -> [Widget ()]
drawUI (State site bookings) = [ui]
    where
        label = str ("Bookings for site " ++ show site)
        ui = B.borderWithLabel label $ C.center content
        content = L.renderList renderElement False bookings

renderElement :: Bool -> CE.Booking -> Widget n
renderElement _ (CE.Booking bookingID bookingStatus) = str (T.unpack bookingID ++ " - " ++ T.unpack bookingStatus)

appEvent :: State -> BrickEvent () Event -> EventM () (Next State)
appEvent s@(State site _) (BT.VtyEvent e) =
    case e of
        V.EvKey V.KEsc _ -> M.halt s
        V.EvKey (V.KChar 'n') _ -> liftIO (CE.sendBooking site) *> M.continue s
        _ -> M.continue s
appEvent s (AppEvent RefreshBookingsEvent) = liftIO updateBookings >>= M.continue
    where
        updateBookings :: IO State
        updateBookings = do
            res <- CE.getBookings siteID
            case res of
                Just bookings -> return $ initialState site bookings
                Nothing -> return s
        State site _ = s
        CE.Site siteID _ _ = site
appEvent l _ = M.continue l

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedAttr,    V.withStyle V.defAttr V.standout)
    ]

app :: M.App State Event ()
app = M.App { M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return
            , M.appAttrMap = const theMap
            }

runBookingsUI :: CE.Site -> [CE.Booking] -> IO ()
runBookingsUI site bookings =
    do
        config <- Graphics.Vty.Config.standardIOConfig
        eventChan <- Brick.BChan.newBChan 10
        _ <- Control.Concurrent.forkIO (sendPeriodicRefresh eventChan)
        _  <- M.customMain
                (Graphics.Vty.mkVty config)
                (Just eventChan)
                app (initialState site bookings)
        return ()

sendPeriodicRefresh :: Brick.BChan.BChan Event -> IO ()
sendPeriodicRefresh chan =
    do
        _ <- Brick.BChan.writeBChan chan RefreshBookingsEvent
        _ <- Control.Concurrent.threadDelay 1000000
        sendPeriodicRefresh chan
