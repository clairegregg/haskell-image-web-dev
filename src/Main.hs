module Main where

import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ (shear (point 0.5 0) <+> circle 0.5, (0, 255, 0)) ]
--exampleDrawing =  [ (shear  (point 0.5 0), polygon [point 0 (-1), point (-0.95) (-0.31), point (-0.59) 0.81, point 0.95 (-0.31), point 0 (-1)], (0, 255, 0)) ]

main = render "output.png" defaultWindow exampleDrawing