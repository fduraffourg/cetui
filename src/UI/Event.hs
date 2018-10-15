module UI.Event
  ( Event(..)
  ) where

import qualified CE.Models as M

data Event
  = PeriodicRefresh
  | SelectSite M.Site
