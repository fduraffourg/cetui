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
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import qualified CE.Client as CEC
import qualified CE.Models as CE
import CE.Vehicle
import CE.Booking
import CE.Core
import qualified UI.Event as UE
import UI.Bookings.Internal
import UI.Status (statusAttr)

data State = State
  { stateSite :: CE.Site
  , stateSiteExtent :: CE.Extent
  , stateBookings :: (L.List () SBooking)
  , stateVehicleNames :: VehicleID -> T.Text
  }

initialState :: CE.Site -> CE.Extent -> [Booking] -> [Vehicle] -> State
initialState site extent bookings vehicles =
  State site extent (L.list () (Vec.fromList sbookings) 1) vehicleNames
  where
    vehicleNames :: VehicleID -> T.Text
    vehicleNames vid =
      fromMaybe (vehicleIDToText vid) $ Map.lookup vid vehiclesMap
    vehiclesMap =
      Map.fromList $ (\v -> (vehicleID v, vehicleName v)) <$> vehicles
    sbookings = listSBookings bookings vehicleNames

drawUI :: State -> Widget ()
drawUI (State site _ bookings _) = ui <=> helpStatus
  where
    label = str ("Bookings for site " ++ show site)
    ui = B.borderWithLabel label $ C.center content
    content = L.renderList renderSBooking False bookings

helpStatus = withAttr statusAttr $ txt "n: new - c: cancel - m: rematch - x: force close"

handleEvent :: State -> BrickEvent () UE.Event -> EventM () (Next State)
handleEvent s@(State (CE.Site siteID _ _) extent list _) (BT.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'n') _ ->
      liftIO (sendBookingDemand siteID extent) *> M.continue s
    V.EvKey (V.KChar 'c') _ -> liftIO (cancelSBooking list) >> M.continue s
    V.EvKey (V.KChar 'm') _ -> liftIO (rematchSBooking list) >> M.continue s
    V.EvKey (V.KChar 'x') _ -> liftIO (forceCloseSBooking list) >> M.continue s
    _ ->
      (\l -> s {stateBookings = l}) <$>
      L.handleListEventVi L.handleListEvent e list >>=
      M.continue
handleEvent s (AppEvent UE.PeriodicRefresh) =
  liftIO (updateSBookings s) >>= M.continue
handleEvent l _ = M.continue l

updateSBookings :: State -> IO State
updateSBookings state@(State site extent list vhcNames) = do
  let CE.Site siteID _ _ = site
  let selected = L.listSelected list
  bookings <- listForSiteFromCE siteID
  let sbookings = listSBookings bookings vhcNames
  let newList = L.listReplace (Vec.fromList sbookings) selected list
  return $ state {stateBookings = newList}

cancelSBooking :: L.List () SBooking -> IO ()
cancelSBooking list =
  case L.listSelectedElement list of
    Just (_, booking) -> recoverIO $ cancelBooking (sbookingID booking)
    Nothing -> return ()

rematchSBooking :: L.List () SBooking -> IO ()
rematchSBooking list =
  case L.listSelectedElement list of
    Just (_, booking) -> recoverIO $ rematchBooking (sbookingID booking)
    Nothing -> return ()

forceCloseSBooking :: L.List () SBooking -> IO ()
forceCloseSBooking list =
  case L.listSelectedElement list of
    Just (_, booking) -> recoverIO $ forceCloseBooking (sbookingID booking)
    Nothing -> return ()

recoverIO :: IO () -> IO ()
recoverIO io = catch io catcher
  where
    catcher :: SomeException -> IO ()
    catcher _ = return ()
