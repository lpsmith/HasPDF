---------------------------------------------------------
-- |
-- Copyright   : (c) 2009-2010 Melding Monads
-- License     : BSD-style
--
-- Maintainer  : leon@melding-monads.com
-- Stability   : experimental
--
-- Two-dimensional coordinates and affine matrices for a
-- PDF document
---------------------------------------------------------

module Graphics.PDF.Coordinates
    ( module Data.Complex
    -- * Angles
    , Degree
    , Radian
    , degree
    -- * Points
    , Scalar
    , Point
    , dot, scalePt
    , project, projectX, projectY
    -- * Transformations
    , Matrix(..)
    , det
    , transform, transform0
    , rotate, translate, scale, spiral
    )
    where

import Data.Complex

type Scalar = Double
type Radian = Scalar
type Degree = Scalar
type Point = Complex Scalar

-- | Converts degrees to radians
degree :: Degree -> Radian
degree x = x * (pi / 180)

{-# SPECIALIZE  dot :: Point -> Point -> Scalar #-}
-- | Dot product of two points
--
-- @dot (x :+ y) (a :+ b) == x * a  +  y * b@
--
-- @dot z w == magnitude z * magnitude w * cos (phase z - phase w)@
dot :: (RealFloat t) => Complex t -> Complex t -> t
dot (x0 :+ y0) (x1 :+ y1) = x0 * x1 + y0 * y1

{-# SPECIALIZE scalePt :: Scalar -> Point -> Point #-}
-- | Scale a point uniformly
scalePt :: (RealFloat t) => t ->  Complex t -> Complex t
scalePt a (x :+ y) = a*x :+ a*y

{-# SPECIALIZE  project :: Point -> Point -> Point #-}
-- | Projects the first point onto the second
project :: (RealFloat t) => Complex t -> Complex t -> Complex t
project z w =  scalePt (dot z w / dot w w) w

{-# SPECIALIZE  projectX :: Point -> Point #-}
-- | Projects a point onto the x-axis
projectX :: (RealFloat t) => Complex t -> Complex t
projectX (x :+ _) = (x :+ 0)

{-# SPECIALIZE  projectY :: Point -> Point #-}
-- | Projects a point onto the y-axis
projectY :: (RealFloat t) => Complex t -> Complex t
projectY (_ :+ y) = (0 :+ y)

-- | An affine transformation matrix.  
-- @
-- Matrix (a :+ b)          a b 0
--        (c :+ d)    ==    c d 0
--        (e :+ f)          e f 1
-- @
data  Matrix
   =  Matrix  !Point  -- ^ X component
              !Point  -- ^ Y component
              !Point  -- ^ translation component
      deriving (Eq, Show)

instance Num Matrix where
    --  Matrix addition
    (+) (Matrix ma mb mc) (Matrix na nb nc) =
         Matrix (ma+na)  (mb+nb)  (mc+nc)
    (*) (Matrix ma mb mc) n =
         Matrix (transform0 ma n) (transform0 mb n) (transform mc n)
    negate (Matrix ma mb mc)  =  Matrix  (-ma)  (-mb)  (-mc)
    abs = error "Graphics.PDF.Coordinates:  abs :: Matrix -> Matrix  is not defined"
    signum = error "Graphics.PDF.Coordinates:  signum :: Matrix -> Matrix  is not defined"
    fromInteger i = Matrix (r :+ 0) (0 :+ r)  (0 :+ 0)
                  where  r = fromInteger i

instance Fractional Matrix where
    recip (Matrix (x0 :+ y0) (x1 :+ y1) (x2 :+ y2))
        = Matrix (x0' :+ y0') (x1' :+ y1') (x2' :+ y2')
          where
            d = x0 * y1 - y0 * x1
            x0' =   y1 / d
            y0' = - y0 / d
            x1' = - x1 / d
            y1' =   x0 / d
            x2' = - (x0' * x2 + x1' * y2)
            y2' = - (y0' * x2 + y1' * y2)

    fromRational x = Matrix (r :+ 0) (0 :+ r) (0 :+ 0)
                   where  r = fromRational x

-- | The determinant of the matrix
det :: Matrix -> Scalar
det (Matrix (x0 :+ y0) (x1 :+ y1) _) = x0 * y1 - y0 * x1

-- | Transforms and translates a point.
transform  :: Point -> Matrix -> Point
transform  (x :+ y) (Matrix ma mb mc) = scalePt x ma + scalePt y mb + mc

-- | Transforms a point without translating it.
-- The zero at the end of the name refers to homogeneous coordinates.
transform0 :: Point -> Matrix -> Point
transform0 (x :+ y) (Matrix ma mb _ ) = scalePt x ma + scalePt y mb

-- | Rotation matrix
rotate :: Radian -> Matrix
rotate = spiral . cis

-- | Translation matrix
--
-- @transform w (translate z) == w + z@
translate :: Point -> Matrix
translate t  = Matrix  (1 :+ 0) (0 :+ 1)  t

-- | @Spiral z@ rotates by @phase z@ and scales by @magnitude z@
--
-- @transform w (spiral z) == w * z@
spiral :: Point -> Matrix
spiral (x :+ y) = Matrix (x :+ y) ((-y) :+ x) (0 :+ 0)

-- | Scaling matrix
scale :: Point -> Matrix
scale (sx :+ sy)  = Matrix (sx :+ 0) (0 :+ sy) (0 :+ 0)
