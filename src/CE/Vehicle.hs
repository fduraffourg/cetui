{-# LANGUAGE OverloadedStrings #-}

module CE.Vehicle
  ( Vehicle(..)
  , VehicleID
  , vehicleIDToText
  ) where

import CE.Core
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T

---------------
-- VehicleID --
---------------
newtype VehicleID =
  VehicleID T.Text
  deriving (Eq, Ord)

vehicleIDToText :: VehicleID -> T.Text
vehicleIDToText (VehicleID t) = t

instance FromJSON VehicleID where
  parseJSON (String t) = return $ VehicleID t
  parseJSON invalid = typeMismatch "VehicleID" invalid

-------------
-- Vehicle --
-------------
data Vehicle = Vehicle
  { vehicleID :: VehicleID
  , vehicleName :: T.Text
  }

instance ListFromCE Vehicle where
  listFromCE = fetchList "http://localhost:9000/v1/vehicles"

instance FromJSON Vehicle where
  parseJSON (Object v) = Vehicle <$> v .: "vehicleID" <*> v .: "name"
  parseJSON invalid = typeMismatch "Vehicle" invalid
