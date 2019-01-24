{-# LANGUAGE OverloadedStrings #-}

module CE.Core
  ( ListFromCE(listFromCE)
  , ListForSiteFromCE(listForSiteFromCE)
  , fetchList
  ) where

import CE.Models (SiteID)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import Network.Wreq

class ListFromCE m where
  listFromCE :: IO [m]

class ListForSiteFromCE m where
  listForSiteFromCE :: SiteID -> IO [m]

fetchList :: FromJSON a => String -> IO [a]
fetchList url = do
  r <- get url
  let jsonValues = r ^.. responseBody . key "result" . values
  let values = sequence $ fromJSON <$> jsonValues
  case values of
    Error msg -> ioError $ userError msg
    Success r -> return r
