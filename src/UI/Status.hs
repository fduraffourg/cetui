{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module UI.Status
  ( drawUI
  , statusAttr
  ) where

import Brick

drawUI :: Widget n
drawUI =
  withAttr statusAttr $ txt "q:quit - 0:DomainChooser - 1:Vehicles - 2:Bookings"

statusAttr = attrName "status-widget"
