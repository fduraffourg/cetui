{-# LANGUAGE OverloadedStrings #-}

module UI.Bookings
  ( initialState
  , State
  , handleEvent
  , drawUI
  ) where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as BT
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import qualified Control.Concurrent
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import qualified CE.Client as CE
import qualified CE.Models as M
import qualified UI.Event as UE

data State =
  State M.Site
        M.Extent
        (L.List () M.Booking)

initialState :: M.Site -> M.Extent -> [M.Booking] -> State
initialState site extent bookings =
  State site extent (L.list () (Vec.fromList bookings) 1)

drawUI :: State -> Widget ()
drawUI (State site _ bookings) = ui
  where
    label = str ("Bookings for site " ++ show site)
    ui = B.borderWithLabel label $ C.center content
    content = L.renderList renderElement False bookings

renderElement :: Bool -> M.Booking -> Widget n
renderElement _ (M.Booking bookingID bookingStatus vehicle) =
  str
    (T.unpack bookingID ++
     " - " ++ T.unpack bookingStatus ++ " - " ++ show vehicle)

handleEvent :: State -> BrickEvent () UE.Event -> EventM () (Next State)
handleEvent s@(State site extent _) (BT.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'n') _ ->
      liftIO (CE.sendBooking site extent) *> M.continue s
    _ -> M.continue s
handleEvent s (AppEvent UE.PeriodicRefresh) =
  liftIO updateBookings >>= M.continue
  where
    updateBookings :: IO State
    updateBookings = do
      bookings <- CE.getBookings siteID
      return $ initialState site extent bookings
    State site extent _ = s
    M.Site siteID _ _ = site
handleEvent l _ = M.continue l
