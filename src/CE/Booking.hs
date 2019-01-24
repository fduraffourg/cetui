{-# LANGUAGE OverloadedStrings #-}

module CE.Booking
  ( BookingID
  , bookingIDToText
  , Booking(..)
  , sendBookingDemand
  , cancelBooking
  , rematchBooking
  , forceCloseBooking
  ) where

import CE.Core
import CE.Vehicle (VehicleID)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Network.Wreq
import System.Random

import CE.Models

---------------
-- BookingID --
---------------
newtype BookingID =
  BookingID T.Text
  deriving (Eq, Ord)

bookingIDToText :: BookingID -> T.Text
bookingIDToText (BookingID t) = t

instance FromJSON BookingID where
  parseJSON (String t) = return $ BookingID t
  parseJSON invalid = typeMismatch "BookingID" invalid

-------------
-- Booking --
-------------
data Booking = Booking
  { bookingID :: BookingID
  , bookingStatus :: T.Text
  , bookingVehicle :: (Maybe VehicleID)
  }

instance FromJSON Booking where
  parseJSON =
    withObject "Booking" $ \v ->
      Booking <$> v .: "bookingID" <*> v .: "status" <*> v .:? "vehicleID"

instance ListForSiteFromCE Booking where
  listForSiteFromCE (SiteID site) =
    fetchList $
    "http://localhost:9000/v1/travel/sites/" ++
    (T.unpack site) ++ "/bookings?size=200"

cancelBooking :: BookingID -> IO ()
cancelBooking bid = do
  let url =
        "http://localhost:9000/v2/travel/bookings/" ++
        (T.unpack $ bookingIDToText bid) ++ "/cancelByOperator"
  _ <- put url BS.empty
  return ()

rematchBooking :: BookingID -> IO ()
rematchBooking bid = do
  let url =
        "http://localhost:9000/v2/travel/bookings/" ++
        (T.unpack $ bookingIDToText bid) ++ "/requestRematch"
  _ <- post url BS.empty
  return ()

forceCloseBooking :: BookingID -> IO ()
forceCloseBooking bid = do
  let url =
        "http://localhost:9000/v2/travel/bookings/" ++
        (T.unpack $ bookingIDToText bid) ++ "/forceClose"
  _ <- post url BS.empty
  return ()

-------------------
-- BookingDemand --
-------------------
data BookingDemand =
  BookingDemand SiteID
                [Path]

instance ToJSON BookingDemand where
  toJSON (BookingDemand siteID path) =
    object
      [ "itinerary" .= path
      , "siteID" .= siteID
      , "userID" .= String "1"
      , "seatCount" .= Number 1
      ]

data Path =
  Path Location
       Location
       SiteID
       DomainID

instance ToJSON Path where
  toJSON (Path start end siteID domainID) =
    object
      [ "start" .= start
      , "end" .= end
      , "siteID" .= siteID
      , "domainID" .= domainID
      ]

sendBookingDemand :: SiteID -> Extent -> IO ()
sendBookingDemand siteID (Extent tl br) = do
  pickUp <- randomRIO (tl, br) :: IO Coordinate
  dropOff <- randomRIO (tl, br) :: IO Coordinate
  let booking =
        BookingDemand
          siteID
          [Path (locationFor pickUp) (locationFor dropOff) siteID domainID]
  res <- post url (toJSON booking)
  return ()
  where
    url = "http://localhost:9000/v1/travel/users/1/bookings"
    locationFor c = Location $ Position srid c
    srid = SRID 4326
    domainID = DomainID 1
