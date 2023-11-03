module Shapes(
  Shape, Point, Vector, Transform, Drawing, Colour,
  point, getX, getY, cr,
  empty, circle, square, ellipse, rectangle, polygon, maskedShape,
  identity, translate, rotate, scale, shear, (<+>),
  insideColour, errorCShape)  where
import Codec.Picture


-- Utilities

data Vector = Vector Double Double
              deriving Show
vector :: Double -> Double -> Vector
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

matrixMul :: Matrix -> Matrix -> Matrix
matrixMul (Matrix (Vector a1 b1) (Vector c1 d1)) (Matrix (Vector a2 b2) (Vector c2 d2)) =
  Matrix (Vector ((a1*a2)+(b1*c2)) ((a1*b2)+(b1*d2))) (Vector ((c1*a2)+(d1*c2)) ((c1*b2)+(d1*d2)))

getX, getY :: Vector -> Double
getX (Vector x _) = x
getY (Vector _ y) = y

-- Defining Points
type Point  = Vector

point :: Double -> Double -> Point
point = vector

-- Basic Shapes - these form the underlying logic of the shapes, but will not be accessed by users
data BaseShape = Empty
           | UnitCircle
           | UnitSquare
           | BasePolygon [ Point ]
           | MaskedImage Shape Shape
          deriving Show

empty, cr, sq :: BaseShape
empty = Empty
cr = UnitCircle
sq = UnitSquare

poly :: [ Point ] -> BaseShape -- Note that polygon requires the list of points be closed (ie the first element is the same as the last element)
poly = BasePolygon

-- Shapes - this is what the user should be interacting with
-- These are basic shapes with some transformation applied to them
type Shape = (Transform, BaseShape)

square :: Double -> Shape
square w = (scale (point w w), sq)

circle :: Double -> Shape
circle r = (scale (point r r), cr)

rectangle :: Double -> Double -> Shape
rectangle w h = (scale (point w h), sq)

ellipse :: Double -> Double -> Shape
ellipse w h = (scale (point w h), cr)

polygon :: [ Point ] -> Shape
polygon v = (identity, poly v)

maskedShape :: Shape -> Shape -> Shape
maskedShape s1 s2 = (identity, MaskedImage s1 s2)

-- Transformations
data Transform = Affine Matrix 
                | Translate Vector
                | Compose Transform Transform
                deriving Show

identity :: Transform
identity = Affine (Matrix (Vector 1 0) (Vector 0 1))
translate, scale, shear :: Vector -> Transform
translate = Translate
scale (Vector x y) = Affine $ matrix x 0 0 y
shear (Vector x y) = Affine $ matrix 1 x y 1
rotate :: Double -> Transform
rotate angle = Affine $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)

(<+>) :: Transform -> Shape -> Shape -- Apply another transformation to a shape
t0 <+> (t1,s) = (Compose t0 t1, s)

transform :: Transform -> Point -> Point
transform (Affine m)                   p = invert m `mult` p
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)

-- Transformation composition
transform (Compose (Affine m1) (Affine m2))       p = transform (Affine $ matrixMul m1 m2) p
transform (Compose (Translate (Vector tx1 ty1)) (Translate (Vector tx2 ty2))) p = transform (Translate (Vector (tx1 + tx2) (ty1 + ty2))) p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Shapes with colours
type Colour = (Pixel8, Pixel8, Pixel8)
type CShape = (Shape, Colour)

-- Drawings
type Drawing = [CShape]

-- interpretation function for drawings
inside1 :: Point -> CShape -> Colour
inside1 p ((t,s),c) = if insides (transform t p) s then c else (0,0,0)

insides :: Point -> BaseShape -> Bool
_ `insides` Empty = False
p `insides` UnitCircle = distance p <= 1
p `insides` UnitSquare = maxnorm  p <= 1
p `insides` (BasePolygon points) = odd (polygonCountIntersects p points)
p `insides` (MaskedImage (t1,s1) (t2,s2)) = (transform t1 p `insides` s1) && (transform t2 p `insides` s2)


insideColour :: Point -> Drawing -> Colour
insideColour p d = firstColour $ map (inside1 p) d -- head $ map (approxinside1 p) d 
                   where firstColour :: [Colour] -> Colour
                         firstColour [] = (0,0,0) -- No drawings? All black
                         firstColour [x]      = x -- Down to the last shape? Use it's colour
                         firstColour ((0,0,0):xs) = firstColour xs -- skip any black colour unless we're at the end
                         firstColour (x:_)    = x -- if you find an "inside" colour return it.

-- Functions for polygon drawing
polygonCountIntersects :: Point -> [Point] -> Int
polygonCountIntersects _ [] = undefined
polygonCountIntersects p [a, b] = if rayIntersects p (a, b) then 1 else 0
polygonCountIntersects p (a:as) = polygonCountIntersects p as + x
                                      where x = if rayIntersects p (a, head as) then 1 else 0

rayIntersects :: Point -> (Point, Point) -> Bool
rayIntersects (Vector x y) (Vector ax ay, Vector bx by) = (x <= min ax bx) && (y >= min ay by) && (y <= max ay by)

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

errorCShape :: CShape
errorCShape = (square 100, (255,0,0))
