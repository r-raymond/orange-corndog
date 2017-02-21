module OrangeCorndog.Intersection
    ( intersectSphereRay
    , intersectPlaneRay
    ) where

import Protolude

import qualified Linear.Affine as LAF
import qualified Linear.V3 as LV3
import qualified Linear.Vector as LVE
import qualified Linear.Metric as LME

import qualified OrangeCorndog.Types as OTY

eps :: OTY.FlType
eps = 0.000001

-- |Calculate the intersection of a Triangle with a Ray
intersectTriangleRay :: OTY.Triangle -> OTY.Ray -> Maybe OTY.Point
intersectTriangleRay (OTY.Triangle p1 p2 p3) (OTY.Ray o d) = do
    let v1 = p2 LAF..-. p1
        v2 = p3 LAF..-. p1
    (v2_x_d, inv_det) <- intersectTRHelper v1 v2 d
    let p1_o = p1 LAF..-. o
    t <- solveLeq p1_o (v1, v2, d) v2_x_d inv_det
    Just (o LAF..+^ t LVE.*^ d)


-- |Solve the linear equation
--      d = M x
-- where M = (a,b,c) as column vectors. Also use the fact that we know bxc and
-- the inverse of the determinant already. Use Gramers rule.
-- The function tests if the first two solutions satisfy
-- x_1 > 0, x_2 > 0, x_1 + x_2 < 1, i.e. the intersection is within the
-- triangle. If that is the case, the third solution is given, if it is
-- positive, bigger than eps.
solveLeq :: OTY.Vec
         -> (OTY.Vec, OTY.Vec, OTY.Vec)
         -> OTY.Vec
         -> OTY.FlType
         -> Maybe OTY.FlType
solveLeq d (a,b,c) bxc inv_det = do
    u <- go1 d bxc inv_det
    (q, v) <- go2 d a c inv_det u
    go3 b q inv_det
      where
        go1 x y id = let pu = (x `LME.dot` y) * id
                     in if pu > 0 && pu < 1
                            then Just pu
                            else Nothing
        go2 x y z id pu = let pq = (x `LV3.cross` y)
                              pv = (pq `LME.dot` z) * id
                          in if pv > 0 && pu + pv < 1
                              then Just (pq, pv)
                              else Nothing
        go3 x y id = let pt = (x `LME.dot` y) * id
                     in if pt > eps
                        then Just pt
                        else Nothing


-- |Calculate the inverse determinant of a 3x3 matrix with given column vectors.
-- Also give back the cross product of the second two columns. Fails if the
-- determinant is smaller than eps.
--
-- Uses the formula M = (a, b, c) => det M = <a, b x c>
intersectTRHelper :: OTY.Vec
                  -> OTY.Vec
                  -> OTY.Vec
                  -> Maybe (OTY.Vec, OTY.FlType)
intersectTRHelper a b c =
    if (abs det) > eps
        then Just (bxc, 1/det)
        else Nothing
      where bxc = b `LV3.cross` c
            det = a `LME.dot` b

intersectPlaneRay :: OTY.Vec -> OTY.Point -> OTY.Ray -> Maybe OTY.Point
intersectPlaneRay n p r = 
    if (abs dn) > eps
        then
            let t = ((p LAF..-. o) `LME.dot` n) / dn
                pt = o LAF..+^ t LVE.*^ d
            in Just pt
        else Nothing
      where
        (o, d) = (OTY.origin r, OTY.direction r)
        dn = d `LME.dot` n


-- |Calculate the intersection of a Sphere with a Ray.
-- This function will return the intersection with the smaller _positive_ time
-- in the description of the ray. Returns `Nothing` if no intersection
intersectSphereRay :: OTY.Sphere -> OTY.Ray -> Maybe OTY.Point
intersectSphereRay (OTY.Sphere c r) ray =
    if disc >= 0
        then let sd = sqrt disc
                 t1 = (-1) * dh + sd
                 t2 = (-1) * dh - sd
                 t  = smallerPositiveValue t2 t1
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
