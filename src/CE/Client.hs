{-# LANGUAGE OverloadedStrings #-}

module CE.Client
  ( getSites
  , getBookings
  , sendBooking
  , getSiteExtent
  , cancelBooking
  , rematchBooking
  ) where

import CE.Models
import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import Data.Vector ((!?))
import Network.Wreq
import System.Random

parseSite :: Value -> Maybe Site
parseSite obj =
  Site <$> (fmap SiteID $ obj ^? key "siteID" . _String) <*> obj ^? key "name" .
  _String <*>
  obj ^?
  key "alias" .
  _String

getSites :: IO (Maybe [Site])
getSites = do
  r <- get "http://localhost:9000/v1/sites"
  let jsonSites = r ^.. responseBody . key "result" . values
  return (sequence $ map parseSite jsonSites)

getSiteExtent :: SiteID -> IO (Maybe Extent)
getSiteExtent (SiteID siteID) = do
  let url =
        "http://localhost:9000/v1/sites/" ++ (T.unpack siteID) ++ "?srid=4326"
  r <- get url
  let jsonExtent =
        r ^? responseBody . key "result" . key "geographic" . key "extent"
  return $ jsonExtent >>= convert
  where
    convert (Array vec) =
      case (vec !? 0, vec !? 1) of
        (Just tl, Just br) -> liftA2 Extent (toCoord tl) (toCoord br)
        _ -> Nothing
    convert _ = Nothing
    toCoord (Array vec) =
      case (vec !? 0, vec !? 1) of
        (Just (Number x), Just (Number y)) ->
          Just $ Coordinate (toRealFloat x) (toRealFloat y)
        _ -> Nothing

getBookings :: SiteID -> IO [Booking]
getBookings (SiteID siteID) = do
  let url =
        "http://localhost:9000/v1/travel/sites/" ++ (T.unpack siteID) ++
        "/bookings?size=1000"
  r <- get url
  json <- asJSON r :: IO (Response (HttpResult [Booking]))
  let (HttpResult bookings) = json ^. responseBody
  return bookings

sendBooking :: Site -> Extent -> IO ()
sendBooking site (Extent tl br) = do
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
    Site siteID _ _ = site
    domainID = DomainID 1

cancelBooking :: Booking -> IO ()
cancelBooking (Booking bookingID _ _) = do
  let url =
        "http://localhost:9000/v2/travel/bookings/" ++ (T.unpack bookingID) ++
        "/cancelByOperator"
  _ <- put url BS.empty
  return ()

rematchBooking :: Booking -> IO ()
rematchBooking (Booking bookingID _ _) = do
  let url =
        "http://localhost:9000/v2/travel/bookings/" ++ (T.unpack bookingID) ++
        "/requestRematch"
  _ <- post url BS.empty
  return ()
