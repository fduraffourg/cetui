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
import Control.Exception
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
handleEvent s@(State site extent list) (BT.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'n') _ ->
      liftIO (CE.sendBooking site extent) *> M.continue s
    V.EvKey (V.KChar 'c') _ -> liftIO (cancelBooking list) >> M.continue s
    _ ->
      State site extent <$> L.handleListEventVi L.handleListEvent e list >>=
      M.continue
handleEvent s (AppEvent UE.PeriodicRefresh) =
  liftIO (updateBookings s) >>= M.continue
handleEvent l _ = M.continue l

updateBookings :: State -> IO State
updateBookings (State site extent list) = do
  let M.Site siteID _ _ = site
  let selected = L.listSelected list
  bookings <- CE.getBookings siteID
  let newList = L.listReplace (Vec.fromList bookings) selected list
  return $ State site extent newList

cancelBooking :: L.List () M.Booking -> IO ()
cancelBooking list =
  case L.listSelectedElement list of
    Just (_, booking) -> recoverIO $ CE.cancelBooking booking
    Nothing -> return ()

recoverIO :: IO () -> IO ()
recoverIO io = catch io catcher
  where
    catcher :: SomeException -> IO ()
    catcher _ = return ()
