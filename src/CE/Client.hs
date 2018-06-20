{-# LANGUAGE OverloadedStrings #-}
module CE.Client (getSites, getBookings, sendBooking) where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import qualified Data.Text as T
import System.Random
import CE.Models


parseSite :: Value -> Maybe Site
parseSite obj = Site
    <$> (fmap SiteID $ obj ^? key "siteID" . _String)
    <*> obj ^? key "name" . _String
    <*> obj ^? key "alias" . _String

getSites :: IO (Maybe [Site])
getSites = do
    r <- get "http://localhost:9000/v1/sites"
    let jsonSites = r ^.. responseBody . key "result" . values
    return (sequence $ map parseSite jsonSites)

getBookings :: SiteID -> IO (Maybe [Booking])
getBookings (SiteID siteID) = do
    let url = "http://localhost:9000/v1/travel/sites/" ++ (T.unpack siteID) ++ "/bookings?size=1000"
    r <- get (url)
    let jsonBookings = r ^.. responseBody . key "result" . values
    return (sequence $ map parseBooking jsonBookings)

parseBooking :: Value -> Maybe Booking
parseBooking obj = Booking
    <$> obj ^? key "bookingID" . _String
    <*> obj ^? key "status" . _String

sendBooking :: Site -> Extent -> IO ()
sendBooking site (Extent tl br) = do
    pickUp <- randomRIO (tl, br) :: IO Coordinate
    dropOff <- randomRIO (tl, br) :: IO Coordinate
    let booking = BookingDemand siteID [Path (locationFor pickUp) (locationFor dropOff) siteID domainID]
    res <- post url (toJSON booking)
    _ <- putStrLn $ show res
    return ()
        where
            url = "http://localhost:9000/v1/travel/users/1/bookings"
            locationFor c = Location $ Position srid c
            srid = SRID 4326
            Site siteID _ _ = site
            domainID = DomainID 1
