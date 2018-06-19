{-# LANGUAGE OverloadedStrings #-}
module CE.Client (SiteID(..), Site(..), getSites, Booking(..), getBookings, sendBooking) where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import qualified Data.Text as T

newtype SiteID = SiteID T.Text deriving (Eq, Show)

data Site = Site
                SiteID -- site ID
                T.Text -- name
                T.Text -- alias
     deriving (Show)

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


data Booking = Booking
                T.Text -- Booking ID
                T.Text -- Status

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

sendBooking :: Site -> IO ()
sendBooking site = return ()
