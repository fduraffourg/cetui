{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import qualified CE.Client as CE
import CE.Core (ListFromCE(..))
import qualified CE.Models
import CE.Vehicle
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

data State = State
  { stateSite :: Maybe CE.Models.Site
  , stateView :: StateView
  , stateVehicles :: [Vehicle]
  }

data StateView
  = StateViewDC DC.State
  | StateViewBO BO.State
  | StateViewMSG String

updateView :: State -> StateView -> State
updateView s v = s {stateView = v}

main :: IO ()
main = do
  eventChan <- Brick.BChan.newBChan 10
  maybeSites <- CE.getSites
  vehicles <- listFromCE
  let state =
        case maybeSites of
          Just sites ->
            State
              Nothing
              (StateViewDC $ DC.initialState eventChan sites)
              vehicles
          Nothing ->
            State
              Nothing
              (StateViewMSG "Failed to get the list of sites")
              vehicles
  config <- Graphics.Vty.Config.standardIOConfig
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
drawUI s = [mainWidget (stateView s) <=> UI.Status.drawUI]
  where
    mainWidget (StateViewDC s) = DC.drawUI s
    mainWidget (StateViewBO s) = BO.drawUI s
    mainWidget (StateViewMSG message) = C.vCenter $ C.hCenter $ str message

appEvent :: State -> BrickEvent () UE.Event -> EventM () (Next State)
appEvent s (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt s
appEvent s (T.AppEvent (UE.SelectSite site)) = switchToSelectedSite s site
appEvent s event = fmap (updateView s) <$> viewEvent (stateView s) event

viewEvent :: StateView -> BrickEvent () UE.Event -> EventM () (Next StateView)
viewEvent (StateViewDC s) event = (StateViewDC <$>) <$> DC.handleEvent s event
viewEvent (StateViewBO s) event = (StateViewBO <$>) <$> BO.handleEvent s event
viewEvent s _ = M.continue s

switchToSelectedSite :: State -> CE.Models.Site -> EventM () (Next State)
switchToSelectedSite state site =
  liftIO getExtent >>= createState >>= refreshNow
  where
    getExtent = CE.getSiteExtent siteID
    createState (Just extent) =
      return $
      state
        { stateSite = Just site
        , stateView = StateViewBO $ BO.initialState site extent [] (stateVehicles state)
        }
    createState Nothing =
      return $ state {stateView = StateViewMSG "Failed to get extent for site"}
    CE.Models.Site siteID _ _ = site
    refreshNow state = appEvent state (T.AppEvent UE.PeriodicRefresh)

sendPeriodicRefresh :: Brick.BChan.BChan UE.Event -> IO ()
sendPeriodicRefresh chan = do
  _ <- Brick.BChan.writeBChan chan UE.PeriodicRefresh
  _ <- Control.Concurrent.threadDelay 1000000
  sendPeriodicRefresh chan
