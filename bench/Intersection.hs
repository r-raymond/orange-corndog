module Main where

import Protolude

import Criterion.Main

import qualified OrangeCorndog.Intersection as OIN
import qualified OrangeCorndog.Types as OTY

sphere :: OTY.Sphere
sphere = OTY.Sphere (OTY.mkP 1000 0 0) 400

ray :: [OTY.FlType] -> [OTY.Ray]
ray dirs = fmap (\x -> OTY.mkRay (OTY.mkP 0 0 0) (OTY.mkP 1 0 x)) dirs

main :: IO ()
main = defaultMain [
    bgroup "Sphere - Ray intersection"
        [ bench "10 intersection" $ nf (fmap (OIN.intersectSphereRay sphere)) (ray [1..10])
        , bench "100 intersection" $ nf (fmap (OIN.intersectSphereRay sphere)) (ray [1..100])
        , bench "1000 intersection" $ nf (fmap (OIN.intersectSphereRay sphere)) (ray [1..1000])
        , bench "10000 intersection" $ nf (fmap (OIN.intersectSphereRay sphere)) (ray [1..10000])
        ]
    ]
