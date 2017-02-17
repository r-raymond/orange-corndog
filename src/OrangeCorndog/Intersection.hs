module OrangeCorndog.Intersection
    ( intersectSphereRay
    ) where

import Protolude

import qualified Linear.Affine as LAF
import qualified Linear.Vector as LVE
import qualified Linear.Metric as LME

import qualified OrangeCorndog.Types as OTY

-- |Calculate the intersection of a Sphere with a Ray.
-- This function will return the intersection with the smaller _positive_ time
-- in the description of the ray. Returns `Nothing` if no intersection
intersectSphereRay :: OTY.Sphere -> OTY.Ray -> Maybe OTY.Point
intersectSphereRay (OTY.Sphere c r) ray =
    if disc >= 0
        then let sd = sqrt disc
                 t1 = (-1) * dh + sd
                 t2 = (-1) * dh - sd
                 t  = smallerPositiveValue t1 t2
                 dist = fmap (d LVE.^*) t
             in
                fmap (o LAF..+^) dist
        else Nothing
  where
    o = OTY.origin ray
    d = OTY.direction ray
    h = o LAF..-. c
    hNorm = LME.quadrance h
    dh = d `LME.dot` h
    disc = dh * dh - hNorm + r * r

-- |Find the smaller but positive value of two numbers. Assumes that the first
-- argument is smaller than the second.
smallerPositiveValue :: (Num a, Ord a) => a -> a -> Maybe a
smallerPositiveValue x y
    | x >= 0 = Just x
    | y >= 0 = Just y
    | otherwise = Nothing
