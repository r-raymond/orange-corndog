module Main where

import Protolude

import Test.Hspec

import qualified OrangeCorndog.Types as OTY
import qualified OrangeCorndog.Intersection as OIN

main :: IO ()
main = hspec $ do
    describe "Intersecting spheres and rays" $ do
        it "sphere ray double intersection" $ do
            let s = OTY.Sphere (OTY.mkP 0 0 0) 1
                r = OTY.mkRay (OTY.mkP 2 0 0) (OTY.mkP (-1) 0 0)
            (OIN.intersectSphereRay s r) `shouldBe` (Just $ OTY.mkP 1 0 0)

        it "sphere ray no intersection" $ do
            let s = OTY.Sphere (OTY.mkP 0 0 0) 1
                r = OTY.mkRay (OTY.mkP 2 0 0) (OTY.mkP 3 0 0)
            (OIN.intersectSphereRay s r) `shouldBe` Nothing

        it "sphere ray single intersection" $ do
            let s = OTY.Sphere (OTY.mkP 0 0 0) 1
                r = OTY.mkRay (OTY.mkP 2 1 0) (OTY.mkP 0 1 0)
            (OIN.intersectSphereRay s r) `shouldBe` (Just $ OTY.mkP 0 1 0)

