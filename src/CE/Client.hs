{-# LANGUAGE OverloadedStrings #-}

module CE.Client
  ( getSites
  , getSiteExtent
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
