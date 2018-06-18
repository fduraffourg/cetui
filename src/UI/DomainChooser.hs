{-# LANGUAGE OverloadedStrings #-}
module UI.DomainChooser (letUserSelectSite) where

import Data.Monoid
import Lens.Micro ((^.))
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import Brick.Widgets.Core
import Brick.Util (fg, on)
import Brick
import qualified Data.Vector as Vec

import qualified CE.Client as CE

data State = State (L.List () CE.Site)

initialState :: [CE.Site] -> State
initialState sites = State (L.list () (Vec.fromList sites) 1)

drawUI :: State -> [Widget ()]
drawUI (State l) = [ui]
    where
        label = str " Sites "
        box = B.borderWithLabel label $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box ]

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then str $ "<" <> s <> ">"
                   else str s
    in C.hCenter $ selStr $ show a

appEvent :: State -> BrickEvent () e -> EventM () (Next State)
appEvent (State l) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt (State $ L.listClear l)
        V.EvKey V.KEnter [] -> M.halt (State l)
        _ -> M.continue =<< State <$> L.handleListEventVi L.handleListEvent e l
appEvent l _ = M.continue l

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedAttr,    V.withStyle V.defAttr V.standout)
    ]

app :: M.App State e ()
app = M.App { M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return
            , M.appAttrMap = const theMap
            }

letUserSelectSite :: [CE.Site] -> IO (Maybe CE.Site)
letUserSelectSite sites =
    do
        State finalList  <- M.defaultMain app (initialState sites)
        return $ fmap snd $ L.listSelectedElement finalList

