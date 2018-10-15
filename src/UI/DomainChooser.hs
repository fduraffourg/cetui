{-# LANGUAGE OverloadedStrings #-}

module UI.DomainChooser
  ( State
  , handleEvent
  , drawUI
  , initialState
  , getSelectedSite
  ) where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import qualified CE.Client as CE
import CE.Models
import qualified UI.Event as UE

data State =
  State (BChan UE.Event)
        (L.List () Site)

initialState :: BChan UE.Event -> [Site] -> State
initialState chan sites = State chan (L.list () (Vec.fromList sites) 1)

drawUI :: State -> Widget ()
drawUI (State _ l) = ui
  where
    label = str " Sites "
    box = B.borderWithLabel label $ L.renderList listDrawElement True l
    ui = C.vCenter $ vBox [C.hCenter box]

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then str $ "<" <> s <> ">"
          else str s
   in C.hCenter $ selStr $ show a

handleEvent :: State -> BrickEvent () UE.Event -> EventM () (Next State)
handleEvent state@(State chan l) (T.VtyEvent (V.EvKey V.KEnter [])) =
  case getSelectedSite state of
    Just site ->
      liftIO (writeBChan chan (UE.SelectSite site)) >> M.continue state
    Nothing -> M.continue state
handleEvent state@(State chan l) (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt state
    _ -> M.continue =<< State chan <$> L.handleListEventVi L.handleListEvent e l
handleEvent l _ = M.continue l

getSelectedSite :: State -> Maybe Site
getSelectedSite (State _ finalList) = snd <$> L.listSelectedElement finalList
