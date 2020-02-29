module Lib where

import           Linear.Vector
import           Linear.V3
import           Linear.Metric
import           Data.List                      ( partition )


type Ray = (V3 Double, V3 Double)
type Triangle = (V3 Double, V3 Double, V3 Double)
type Mesh = [Triangle]

rayTriangleIntersection :: Ray -> Triangle -> Bool
rayTriangleIntersection (origin, direction) (a, b, c) =
  t >= 0 && u >= 0 && v >= 0 && (u + v) <= 1
 where
  e1     = b - a
  e2     = c - a
  normal = e1 `cross` e2
  det    = -(direction `dot` normal)
  ao     = origin - a
  dao    = ao `cross` direction
  invdet = 1 / det
  u      = (e2 `dot` dao) * invdet
  v      = (-(e1 `dot` dao)) * invdet
  t      = (ao `dot` normal) * invdet


triangleOreintedCorrectly :: Mesh -> Triangle -> Bool
triangleOreintedCorrectly mesh t@(a, b, c) =
  length normIntersections `mod` 2 == 0
 where
  centroid = (a + b + c) ^/ 3
  norm     = (b - a) `cross` (c - a)
  normIntersections =
    filter (rayTriangleIntersection (centroid, norm)) (filter (/= t) mesh)

meshOreintations :: Mesh -> (Mesh, Mesh)
meshOreintations mesh = partition (triangleOreintedCorrectly mesh) mesh
