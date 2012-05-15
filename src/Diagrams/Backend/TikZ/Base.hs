{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--module Graphics.Rendering.Canvas 
module Diagrams.Backend.TikZ.Base
  ( Render(..)
  , renderWith
  , withTikZSurface
  , ellipse
  , renderTikZ
  , rawRenderTikZ
  , fixedSegment
  , curvedFixedSegment

  , newPath
  , moveTo
  , lineTo
  , curveTo
  , closePath
  , stroke
  , setDash
  , fillColor
  , fillCommand
  , drawCommand
  , strokeColor
  , lineWidth
  , lineCap
  , lineJoin
  , epsilon
  , showP
  ) where

import Diagrams.Attributes(Color(..),LineCap(..),LineJoin(..))
import Diagrams.TwoD.Types
import Control.Monad.Writer
import Data.DList(DList,toList,fromList)
import System.IO (openFile, hPutStr, IOMode(..), hClose)

newtype Render m = Render { runRender :: WriterT (DList String) IO m }
  deriving (Functor, Monad, MonadWriter (DList String))

data Surface = Surface { header :: String, footer :: String, fileName :: String } 

renderWith :: MonadIO m => Surface -> Render a -> m a
renderWith s r = liftIO $ do (v,ss) <- runWriterT $ (runRender r)
                             h <- openFile (fileName s) WriteMode
                             hPutStr h (header s)
                             mapM_ (hPutStr h) (toList ss)
                             hPutStr h (footer s)
                             hClose h
                             return v

withTikZSurface :: String -> (Surface -> IO a) -> IO a
withTikZSurface file f = f s
  where s = Surface tikzHeader tikzFooter file

rawRenderTikZ :: String -> Render ()
rawRenderTikZ s = tell $ fromList [s]

renderTikZ :: String -> Render ()
renderTikZ s = tell $ fromList [texPrefix, s, ";\n"]

--mkTikZCall :: Show a => String -> [a] -> Render()
--mkTikZCall n vs = renderTikZ . concat $ [n, "("] ++ intersperse "," (map show vs) ++ [")"]

ellipse :: R2 -> Double -> Double -> Rad -> Render ()
ellipse center h v rad = rawRenderTikZ $ " \\path [shift={"
                           ++ showP center ++ "}] [rotate=" ++ showD4 (getDeg deg) ++ "] (0,0) ellipse ("
                           ++ showD4 h ++ " and " ++ showD4 v ++ ")"
                               where
                                 deg = convertAngle rad :: Deg

newPath :: Render ()
newPath = rawRenderTikZ "\\path "

closePath :: Render ()
closePath = rawRenderTikZ " -- cycle "

moveTo :: R2 -> Render ()
moveTo v = rawRenderTikZ $ " " ++ showP v ++ " "

lineTo :: R2 -> Render ()
lineTo v = rawRenderTikZ $ " -- " ++ showP v ++ " "

curveTo :: R2 -> R2 -> R2 -> Render ()
curveTo v2 v3 v4 = rawRenderTikZ $ " .. controls " ++ showP v2 ++ " and " ++ showP v3 ++ " .. " ++ showP v4 ++ " "

stroke :: Render ()
stroke = rawRenderTikZ "; "

showColorTikZ :: (Color c) => c -> String
showColorTikZ c = showD4 r ++ "," ++ showD4 g ++ "," ++ showD4 b
  where (r,g,b,_a) = colorToRGBA c

fillColor :: (Color c) => c -> Render ()
fillColor c = rawRenderTikZ $ " \\definecolor{fc}{rgb}{" ++ showColorTikZ c ++ "} \\pgfsetfillcolor{fc}"

fillCommand :: (Color c) => c -> Render ()
fillCommand _c = rawRenderTikZ $ " [fill] "

drawCommand :: Double -> Render ()
drawCommand w
    | w < epsilon  = rawRenderTikZ ""
    | otherwise    = rawRenderTikZ " [draw]"

strokeColor :: (Color c) => c -> Render ()
strokeColor c = rawRenderTikZ $ " \\definecolor{sc}{rgb}{" ++ showColorTikZ c ++ "} \\pgfsetstrokecolor{sc}"

-- about 28.5pt = 1cm.  Get the official value.
numPtsPerCm :: Double
numPtsPerCm = 28.5

-- a number (line width) below this is regarded as zero
epsilon :: Double
epsilon = 0.001

convertCmToPt :: Double -> Double
convertCmToPt x = numPtsPerCm * x

lineWidth :: Double -> Render ()
lineWidth w = rawRenderTikZ $
    "[line width=" ++ showD4 (convertCmToPt w) ++ "] "

setDash :: [Double] -> Double -> Render ()
setDash ds offs
    = rawRenderTikZ $ "[dash pattern="
      ++ concat (zipWith (++) (cycle [" on "," off "]) (map (show . convertCmToPt) ds))
             ++ "] [dash phase=" ++ show (convertCmToPt offs)
             ++ "] "

lineCap :: LineCap -> Render ()
lineCap lc = rawRenderTikZ $
    "[line cap=" ++ fromLineCap lc ++ "] "

lineJoin :: LineJoin -> Render ()
lineJoin lj = rawRenderTikZ $
    "[line join=" ++ fromLineJoin lj ++ "] "

fromLineCap :: LineCap -> String
fromLineCap LineCapRound  = show "round"
fromLineCap LineCapSquare = show "rect"
fromLineCap _             = show "butt"

fromLineJoin :: LineJoin -> String
fromLineJoin LineJoinRound = show "round"
fromLineJoin LineJoinBevel = show "bevel"
fromLineJoin _             = show "miter"

fixedSegment :: R2 -> R2 -> Render ()
fixedSegment v1 v2 = rawRenderTikZ $ " " ++ showP v1 ++ " -- " ++ showP v2 ++ ";\n"

curvedFixedSegment :: R2 -> R2 -> R2 -> R2 -> Render ()
curvedFixedSegment v1 v2 v3 v4 = rawRenderTikZ $ "\\draw " ++ showP v1 ++ " .. controls " ++ showP v2 ++ " and " ++ showP v3 ++ " .. " ++ showP v4 ++ ";\n"

texPrefix :: String
texPrefix = "\\"

tikzHeader :: String
tikzHeader = "\\begin{tikzpicture}\n"

tikzFooter :: String
tikzFooter = "\\end{tikzpicture}\n"

showD :: Int -> Double -> String
showD n x = show $ ((fromInteger (round ((x * 10.0^n):: Double)) / 10.0^n)::Double)

showD4 :: Double -> String
showD4 = showD 4

showP :: R2 -> String
showP = showP' . unr2

showP' :: (Double,Double) -> String
showP' (x,y) = "(" ++ showD4 x ++ "," ++ showD4 y ++ ")"
