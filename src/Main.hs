{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import qualified CE.Client as CE
import qualified CE.Models
import qualified UI.Bookings as BO
import qualified UI.DomainChooser as DC
import qualified UI.Event as UE
import qualified UI.Status

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Config

import Control.Concurrent
import Control.Monad.IO.Class

data State
  = StateDC DC.State
  | StateBO BO.State
  | Message String

main :: IO ()
main = do
  maybeSites <- CE.getSites
  let state =
        case maybeSites of
          Just sites -> StateDC $ DC.initialState sites
          Nothing -> Message "Failed to get the list of sites"
  config <- Graphics.Vty.Config.standardIOConfig
  eventChan <- Brick.BChan.newBChan 10
  forkIO (sendPeriodicRefresh eventChan)
  M.customMain (V.mkVty config) (Just eventChan) app state
  return ()

app :: M.App State UE.Event ()
app =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theMap
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.withStyle V.defAttr V.standout)
    , (UI.Status.statusAttr, V.withStyle V.defAttr V.reverseVideo)
    ]

drawUI :: State -> [Widget ()]
drawUI s = [mainWidget s <=> UI.Status.drawUI]
  where
    mainWidget (StateDC s) = DC.drawUI s
    mainWidget (StateBO s) = BO.drawUI s
    mainWidget (Message message) = C.vCenter $ C.hCenter $ str message

appEvent :: State -> BrickEvent () UE.Event -> EventM () (Next State)
appEvent s (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt s
appEvent (StateDC s) (T.VtyEvent (V.EvKey V.KEnter [])) =
  case DC.getSelectedSite s of
    Just site -> switchToSelectedSite site
    Nothing -> M.continue (StateDC s)
appEvent (StateDC s) event = (StateDC <$>) <$> DC.handleEvent s event
appEvent (StateBO s) event = (StateBO <$>) <$> BO.handleEvent s event
appEvent s _ = M.continue s

switchToSelectedSite :: CE.Models.Site -> EventM () (Next State)
switchToSelectedSite site = liftIO getExtent >>= createState >>= refreshNow
  where
    getExtent = CE.getSiteExtent siteID
    createState (Just extent) =
      return $ StateBO $ BO.initialState site extent []
    createState Nothing = return $ Message "Failed to get extent for site"
    CE.Models.Site siteID _ _ = site
    refreshNow state = appEvent state (T.AppEvent UE.PeriodicRefresh)

sendPeriodicRefresh :: Brick.BChan.BChan UE.Event -> IO ()
sendPeriodicRefresh chan = do
  _ <- Brick.BChan.writeBChan chan UE.PeriodicRefresh
  _ <- Control.Concurrent.threadDelay 1000000
  sendPeriodicRefresh chan
