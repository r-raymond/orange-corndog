module Main where

import Protolude

import Test.Hspec

import qualified Linear.Affine as LAF
import qualified Linear.V3     as LV3

import qualified OrangeCorndog.Types as OTY
import qualified OrangeCorndog.Intersection as OIN

main :: IO ()
main = hspec $ do
    describe "Intersecting spheres and rays" $ do
        it "sphere ray double intersection" $ do
            let s = OTY.Sphere (LAF.P $ LV3.V3 0 0 0) 1
                r = OTY.newRay (LAF.P $ LV3.V3 2 0 0) (LAF.P $ LV3.V3 (-1) 0 0)
            (OIN.intersectSphereRay s r) `shouldBe` (Just $ LAF.P $ LV3.V3 1 0 0)

        it "sphere ray no intersection" $ do
            let s = OTY.Sphere (LAF.P $ LV3.V3 0 0 0) 1
                r = OTY.newRay (LAF.P $ LV3.V3 2 0 0) (LAF.P $ LV3.V3 3 0 0)
            (OIN.intersectSphereRay s r) `shouldBe` Nothing

        it "sphere ray single intersection" $ do
            let s = OTY.Sphere (LAF.P $ LV3.V3 0 0 0) 1
                r = OTY.newRay (LAF.P $ LV3.V3 2 1 0) (LAF.P $ LV3.V3 0 1 0)
            (OIN.intersectSphereRay s r) `shouldBe` (Just $ LAF.P $ LV3.V3 0 1 0)

