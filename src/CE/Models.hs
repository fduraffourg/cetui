{-# LANGUAGE OverloadedStrings #-}
module CE.Models
    ( Site(..)
    , SiteID(..)
    , Location(..)
    , Position(..)
    , SRID(..)
    , Extent(..)
    , Coordinate(..)
    , DomainID(..)
    , HttpResult(..)
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


data Location = Location Position

instance ToJSON Location where
    toJSON (Location position) = object ["position" .= position]


data Position = Position SRID Coordinate

instance ToJSON Position where
    toJSON (Position srid coordinate) = object [ "srid" .= srid, "coordinate" .= coordinate]


data SRID = SRID Integer

instance ToJSON SRID where
    toJSON (SRID int) = Number $ fromInteger int


data HttpResult a = HttpResult a

instance FromJSON a => FromJSON (HttpResult a) where
    parseJSON = withObject "Result" $ \v -> HttpResult <$> v .: "result"
