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

    describe "Intersecting triangles and rays" $ do
        let t = OTY.mkTriangle (OTY.mkP 0 2 0) (OTY.mkP (-2) 0 0) (OTY.mkP 2 0 0)
        it "parallel triangle ray" $ do
            let r = OTY.mkRay (OTY.mkP 0 0 1) (OTY.mkP 1 0 1)
            (OIN.intersectTriangleRay t r) `shouldBe` Nothing
        it "parallel triangle ray in same plane" $ do
            let r = OTY.mkRay (OTY.mkP 0 0 0) (OTY.mkP 1 1 0)
            (OIN.intersectTriangleRay t r) `shouldBe` Nothing
        it "triangle ray hitting in interior" $ do
            let r = OTY.mkRay (OTY.mkP 0 1 1) (OTY.mkP 0 1 0.5)
            (OIN.intersectTriangleRay t r) `shouldBe` (Just $ OTY.mkP 0 1 0)
        it "triangle ray wrong side" $ do
            let r = OTY.mkRay (OTY.mkP 0 1 1) (OTY.mkP 0 1 1.5)
            (OIN.intersectTriangleRay t r) `shouldBe` Nothing
        it "triangles ray corner case" $ do
            let r =  OTY.mkRay (OTY.mkP 0 2 1) (OTY.mkP 0 2 0.5)
            (OIN.intersectTriangleRay t r) `shouldBe` (Just $ OTY.mkP 0 2 0)

