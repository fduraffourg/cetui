{-# LANGUAGE OverloadedStrings #-}

module UI.Bookings.Internal
  ( SBooking(..)
  , renderSBooking
  , listSBookings
  ) where

import Brick
import CE.Booking
import CE.Vehicle
import qualified Data.Text as T

data SBooking = SBooking
  { sbookingID :: BookingID
  , sbookingStatus :: T.Text
  , sbookingVehicle :: T.Text
  }

renderSBooking :: Bool -> SBooking -> Widget n
renderSBooking _ (SBooking bid status vehicle) =
  (str . T.unpack . (T.intercalate " - "))
    [bookingIDToText bid, status, vehicle]

listSBookings :: [Booking] -> (VehicleID -> T.Text) -> [SBooking]
listSBookings bookings vhcName = buildSBooking <$> bookings
  where
    buildSBooking b =
      SBooking (bookingID b) (bookingStatus b) (convertVID $ bookingVehicle b)
    convertVID (Just vid) = vhcName vid
    convertVID Nothing = "no vehicle"
