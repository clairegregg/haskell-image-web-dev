module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY, cr,
  empty, circle, square, rectangle, polygon,
  identity, translate, rotate, scale, shear, (<+>),
  insideColour)  where
import Codec.Picture


-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector

data Shape = Empty
           | Circle
           | Square
           | Polygon [ Point ]
          deriving Show

empty, cr, sq :: Shape

empty = Empty
cr = Circle
sq = Square

-- Note that polygon requires the list of points be closed (ie the first element is the same as the last element)
polygon :: [Point] -> Shape
polygon = Polygon

square :: Double -> (Transform, Shape)
square w = (scale (point w w), sq)

circle :: Double -> (Transform, Shape)
circle r = (scale (point r r), cr)

rectangle :: Double -> Double -> (Transform, Shape)
rectangle w h = (scale (point w h), sq)

ellipse :: Double -> Double -> (Transform, Shape)
ellipse w h = (scale (point w h), cr)

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Shear Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
shear = Shear
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> (t1,s) = (Compose t0 t1, s)

transform :: Transform -> Point -> Point
transform Identity                   x = x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Shear (Vector tx ty)) (Vector px py) = Vector (px + (tx * py)) (py + (ty * px))
transform (Rotate m)                 p = invert m `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Complex shapes
type Colour = (Pixel8, Pixel8, Pixel8)
type CShape = ((Transform,Shape),Colour)

-- Drawings

type Drawing = [CShape]

-- interpretation function for drawings
--inside :: Point -> Drawing -> Bool
--inside p d = any (inside1 p) d

inside1 :: Point -> CShape -> Colour
inside1 p ((t,s),c) = if insides (transform t p) s then c else (0,0,0)

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
p `insides` (Polygon points) = even (polygonCountIntersects p points)

insideColour :: Point -> Drawing -> Colour
insideColour p d = firstColour $ map (inside1 p) d -- head $ map (approxinside1 p) d 
                   where firstColour :: [Colour] -> Colour
                         firstColour [] = (0,0,0)
                         firstColour [x]      = x -- Down to the last shape? Use it's colour
                         firstColour ((0,0,0):xs) = firstColour xs -- skip any 100's unless we're at the end
                         firstColour (x:_)    = x -- if you find an "inside" colour return it.

-- Functions for polygon drawing
polygonCountIntersects :: Point -> [Point] -> Int
polygonCountIntersects point [] = undefined
polygonCountIntersects point [a, b] = if rayIntersects point (a, b) then 1 else 0
polygonCountIntersects point (a:as) = polygonCountIntersects point as + x
                                      where x = if rayIntersects point (a, head as) then 1 else 0

rayIntersects :: Point -> (Point, Point) -> Bool
rayIntersects (Vector x y) (Vector ax ay, Vector bx by) = (x <= min ax bx) && (y >= min ay by) && (y <= max ay by)

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

testShape = (scale (point 10 10), circle)
