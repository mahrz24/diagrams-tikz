{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.TikZ

p = fromVertices $ map p2 [(-4,0),(0,0),(0,2)]

charge = circle 0.1 # fc black

fieldPoint = circle 0.05 # fc black

rc1     = p2 ( 0,2)
rc2     = p2 (-4,0)

d = position [(rc1,charge)
             ,(rc2,charge)
             ,(origin,fromVertices [origin,rc1])
             ,(origin,fromVertices [origin,rc2])
             ,(origin,fieldPoint)
             ,(p2 (0,2.3),text "$Q_1 = +4.0 \\mu\\rm C$")
             ,(p2 (-4,-0.3),text "$Q_2 = -8.0 \\mu\\rm C$")
             ,(p2 (0.2,-0.2),text "P")
             ,(p2 (0.5,1), text "2 cm")
             ,(p2 (-2,0.3), text "4 cm")
             ]

main = renderDia TikZ (TikZOptions "EField.tex" (Dims 6 6) LaTeX) d