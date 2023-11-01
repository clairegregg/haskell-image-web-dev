module Render(Window,defaultWindow,samples,render) where
import Codec.Picture
import Shapes


--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered,
--             and the size of the output device to render into
data Window = Window Point Point (Int,Int)

-- Default window renders a small region around the origin into
-- a 50x50 pixel image
defaultWindow :: Window
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (500,500)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / fromIntegral (n-1) .. ]

-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ]

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]

-- render a drawing into an image, then save into a file
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win

      pixRenderer :: Int -> Int -> PixelRGB8
      pixRenderer x y = PixelRGB8 r g b where (r,g,b) = colorForImage ((pixels win!!y)!!x)
      
      colorForImage p = p `insideColour` sh

