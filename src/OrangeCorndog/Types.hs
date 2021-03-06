module OrangeCorndog.Types
    ( FlType
    , Vec, mkV
    , Point, mkP
    , mkRay
    , Ray(..)
    , Sphere(..)
    , Triangle(..)
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
mkTriangle = Triangle

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
    }
