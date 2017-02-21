module OrangeCorndog.Types
    ( FlType
    , Vec, mkV
    , Point, mkP
    , mkRay
    , Ray(origin, direction)
    , Sphere(..)
    , Triangle(p1, p2, p3, normal)
    , mkTriangle
    ) where

import           Protolude

import qualified Linear.Affine as LAF
import qualified Linear.Metric as LME
import qualified Linear.V3     as LV3

type FlType = Float

type Vec = LV3.V3 FlType
type Point = LAF.Point LV3.V3 FlType

mkP :: FlType -> FlType -> FlType -> Point
mkP x y z = LAF.P $ LV3.V3 x y z

mkV :: FlType -> FlType -> FlType -> Vec
mkV = LV3.V3

mkRay :: Point -> Point -> Ray
mkRay a b = Ray a dir
  where
    dir = LME.signorm (b LAF..-. a)

mkTriangle :: Point -> Point -> Point -> Triangle
mkTriangle a b c = Triangle a b c n
  where
    v1 = a LAF..-. b
    v2 = a LAF..-. c
    n  = LV3.cross v1 v2

data Ray
    = Ray
    { origin    :: Point
    , direction :: Vec      -- ^ has to be normed
    }

data Sphere
    = Sphere
    { center :: Point
    , radius :: FlType
    }

data Triangle
    = Triangle
    { p1 :: Point
    , p2 :: Point
    , p3 :: Point
    , normal :: Vec
    }
