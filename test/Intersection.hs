module Main where

import Protolude

import Test.Hspec

import qualified Linear.Metric as LME
import qualified Linear.Affine as LAF
import qualified Linear.V3     as LV3

import qualified OrangeCorndog.Types as OTY
import qualified OrangeCorndog.Intersection as OIN

--eps :: OTY.FlType
--eps = 0.001
--
---- |Are two `FlType` close enough?
--flEqual :: OTY.FlType -> OTY.FlType -> Bool
--flEqual f1 f2 = abs (f1 - f2) < eps
--
---- |Are two `OTY.Point`s close enough?
--ptEqual :: OTY.Point -> OTY.Point -> Bool
--ptEqual p1 p2 = LME.norm (p2 LAF..-. p1) < eps

main :: IO ()
main = hspec $ do
    describe "Intersecting spheres and rays" $ do
        it "sphere ray 1" $ do
            let s = OTY.Sphere (LAF.P $ LV3.V3 0 0 0) 1
                r = OTY.newRay (LAF.P $ LV3.V3 2 0 0) (LAF.P $ LV3.V3 (-1) 0 0)
            (OIN.intersectSphereRay s r) `shouldBe` (Just $ LAF.P $ LV3.V3 1 0 0)
