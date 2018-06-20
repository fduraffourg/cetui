{-# LANGUAGE OverloadedStrings #-}
module CE.Models
    ( Site(..)
    , SiteID(..)
    , BookingDemand(..)
    , Path(..)
    , Location(..)
    , Position(..)
    , SRID(..)
    , Booking(..)
    , Extent(..)
    , Coordinate(..)
    , DomainID(..)
    ) where

import Data.Aeson
import System.Random
import qualified Data.Text as T
import qualified Data.Scientific as S
import qualified Data.Vector as Vec

data SiteID = SiteID T.Text deriving (Eq, Show)

instance ToJSON SiteID where
    toJSON (SiteID siteID) = String siteID


data DomainID = DomainID Integer

instance ToJSON DomainID where
    toJSON (DomainID domainID) = Number $ fromInteger domainID


data Site = Site
                SiteID -- site ID
                T.Text -- name
                T.Text -- alias
     deriving (Show)


data Extent = Extent Coordinate Coordinate


data Coordinate = Coordinate Double Double

instance Random Coordinate where
    randomR (a, b) gen = (Coordinate x y, finalGen)
        where
            (x, ng) = randomR (xa, xb) gen
            (y, finalGen) = randomR (ya, yb) ng
            Coordinate xa ya = a
            Coordinate xb yb = b
    random gen = (Coordinate x y, finalGen)
        where
            (x, ng) = random gen
            (y, finalGen) = random ng

instance ToJSON Coordinate where
    toJSON (Coordinate x y) = Array $ Vec.fromList coordinates
        where coordinates = fmap (Number . S.fromFloatDigits) [x, y]


data BookingDemand = BookingDemand SiteID [Path]

instance ToJSON BookingDemand where
    toJSON (BookingDemand siteID path) =
        object
            [ "itinerary" .= path
            , "siteID" .= siteID
            , "userID" .= String "1"
            , "seatCount" .= Number 1
            ]


data Path = Path Location Location SiteID DomainID

instance ToJSON Path where
    toJSON (Path start end siteID domainID) =
        object [ "start" .= start
               , "end" .= end
               , "siteID" .= siteID
               , "domainID" .= domainID
               ]


data Location = Location Position

instance ToJSON Location where
    toJSON (Location position) = object ["position" .= position]


data Position = Position SRID Coordinate

instance ToJSON Position where
    toJSON (Position srid coordinate) = object [ "srid" .= srid, "coordinate" .= coordinate]


data SRID = SRID Integer

instance ToJSON SRID where
    toJSON (SRID int) = Number $ fromInteger int


data Booking = Booking
                T.Text -- Booking ID
                T.Text -- Status
                (Maybe T.Text) -- Vehicle
