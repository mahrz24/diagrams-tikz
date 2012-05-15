{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.TikZ

hilbert :: [Trail R2]
hilbert = iterate genHilbert mempty
  where genHilbert t = let t' = reverseTrail t
                       in  mconcat [ rotateBy (-1/4) t'
                                   , fromOffsets [unitY]
                                   , t
                                   , fromOffsets [unitX]
                                   , t
                                   , fromOffsets [negateV unitY]
                                   , rotateBy (1/4) t'
                                   ]

d = (hilbert !! 6)
  # strokeT
  # lw (0.1*0.2)
  # fc red
  # centerXY
  # pad 1.1

--main = defaultMain d
main = renderDia TikZ (TikZOptions "Hilbert1.tex" (Dims 14 14) LaTeX) d
