{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , DeriveDataTypeable
           , ViewPatterns
  #-}
{-|
  A TikZ backend.
-}

module Diagrams.Backend.TikZ

  ( TikZ(..) -- rendering token
  , Options(..) -- for rendering options specific to TikZ
  , OutputFormat(..) -- output format options
  , absoluteTrail
  ) where

import qualified Diagrams.Backend.TikZ.Base as L

import Diagrams.Prelude hiding (r2)

import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Text
import Graphics.Rendering.Diagrams.Points

import Control.Monad.Identity
import Data.Maybe (catMaybes)
import qualified Data.Foldable as F
import Data.Typeable

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data TikZ = TikZ
    deriving (Show, Typeable)

data OutputFormat = LaTeX | ConTeXt

instance Monoid (Render TikZ R2) where
  mempty  = T $ return ()
  (T r1) `mappend` (T r2) = T (r1 >> r2)

instance Backend TikZ R2 where
  data Render  TikZ R2 = T (L.Render ())
  type Result  TikZ R2 = IO ()
  data Options TikZ R2 = TikZOptions
                        { fileName     :: String       -- ^ the name of the file you want generated
                        , size :: SizeSpec2D           -- ^ The requested size.
                        , outputFormat :: OutputFormat -- ^ The output format.
                        }

  withStyle _ s t (T r) = T $ do
    L.rawRenderTikZ "\\begin{scope}\n"
    tikzStyle s
    r
    tikzStylePost s
    drawOrNot s
    L.stroke
    L.rawRenderTikZ "\\end{scope}\n"

  doRender _ options (T r) =
      let (w,h) = case size options of
                  Width w'   -> (w',w')
                  Height h'  -> (h',h')
                  Dims w' h' -> (w',h')
                  Absolute   -> (1,1)
          surfaceF surface = L.renderWith surface r
      in  L.withTikZSurface (fileName options) surfaceF


  adjustDia c opts d = adjustDia2D size setTikZSize c opts (d # reflectY                                                          )
    where setTikZSize sz o = o { size = sz }

renderT :: (Renderable a TikZ, V a ~ R2) => a -> L.Render ()
renderT a = case (render TikZ a) of T r -> r

tikzStyle :: Style v -> L.Render ()
tikzStyle s = foldr (>>) (return ())
              . catMaybes $ [ handle fColor
                            , handle lColor
                            ]
    where handle :: (AttributeClass a) => (a -> L.Render ()) -> Maybe (L.Render ())
          handle f = f `fmap` getAttr s
          lColor = L.strokeColor . getLineColor
          fColor = L.fillColor . getFillColor

tikzStylePost :: Style v -> L.Render ()
tikzStylePost s = foldr (>>) (return ())
                  . catMaybes $ [ handle lWidth
                                , handle lJoin
                                , handle lCap
                                , handle lDashing
                                , handle fCommand
                                ]
    where handle :: (AttributeClass a) => (a -> L.Render ()) -> Maybe (L.Render ())
          handle f = f `fmap` getAttr s
          fCommand = L.fillCommand . getFillColor
          lWidth = L.lineWidth . getLineWidth
          lCap   = L.lineCap . getLineCap
          lJoin  = L.lineJoin . getLineJoin
          lDashing (getDashing -> Dashing ds offs) =
              L.setDash ds offs

--isLineWidthSet :: Style v -> Bool
--isLineWidthSet s = case (getAttr s :: Maybe LineWidth) of
--                     Nothing -> False
--                     Just _  -> True
--                         -- where
--                         --   getA :: Style v -> Maybe LineWidth
--                         --   getA = getAttr

drawOrNot :: Style v -> L.Render ()
drawOrNot s = case (fmap getLineWidth (getAttr s)) of
                Nothing -> L.rawRenderTikZ " [draw]"
                Just x  -> case x < L.epsilon of
                             True  -> L.rawRenderTikZ ""
                             False -> L.rawRenderTikZ " [draw]"

--tikzTransf :: T2 -> L.Render ()
--tikzTransf t = L.rawRenderTikZ m
--    where m = "[cm={" ++ show a1 ++ "," ++ show b1 ++ "," ++ show a2 ++ "," ++ show b2 ++ "," ++ show (c1,c2) ++ "}]\n"
--          (a1,a2) = DT.apply t (1,0)
--          (b1,b2) = DT.apply t (0,1)
--          (c1,c2) = DT.transl t


instance Renderable (Segment R2) TikZ where
  render _ (Linear v) = T $ L.lineTo v
  render _ (Cubic v1 v2 v3) = T $ L.curveTo v1 v2 v3

instance Renderable (FixedSegment R2) TikZ where
  render _ (FLinear (P v1) (P v2)) = T $ L.fixedSegment v1 v2
  render _ (FCubic (P v1) (P v2) (P v3) (P v4)) = T $ L.curvedFixedSegment v1 v2 v3 v4

instance Renderable (Trail R2) TikZ where
  render _ (Trail segs c) = T $ do
    mapM_ renderT segs
    when c $ L.closePath

instance Renderable (Path R2) TikZ where
  render _ (Path trs) = T $ L.newPath >> F.mapM_ renderTrail trs
    where renderTrail (P p, tr) = do
            L.moveTo p
            renderT (absoluteTrail p tr)

absoluteTrail :: R2 -> Trail R2 -> Trail R2
absoluteTrail v (Trail segs c) = Trail (absolute v segs) c

absolute :: R2 -> [Segment R2] -> [Segment R2]
absolute _ [] = []
absolute v (s:ss) = s' : absolute v' ss
  where (v',s') = addV s
        addV (Linear a) = (\p -> (p, Linear p)) (a ^+^ v)
        addV (Cubic a b c) = (c ^+^ v, Cubic (a ^+^ v) (b ^+^ v) (c ^+^ v))

instance Renderable Text TikZ where
  render _ (Text tr ta str) = T $ L.rawRenderTikZ $ " \\path " ++ L.showP (transl tr) ++ " node {" ++ str ++ "} "
