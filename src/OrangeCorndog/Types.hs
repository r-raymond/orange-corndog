module OrangeCorndog.Types
    ( FlType
    , Vec
    , Point
    , newRay
    , Ray(origin, direction)
    , Sphere(..)
    ) where

import           Protolude

import qualified Linear.Affine as LAF
import qualified Linear.Metric as LME
import qualified Linear.V3     as LV3

type FlType = Float

type Vec = LV3.V3 FlType
type Point = LAF.Point LV3.V3 FlType

newRay :: Point -> Point -> Ray
newRay a b = Ray a dir
  where
    dir = LME.signorm (b LAF..-. a)

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
