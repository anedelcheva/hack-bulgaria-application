fromCubicCmToL :: Fractional a => a -> a
fromCubicCmToL n = n / 1000

volumeOfTetrahedron :: Floating a => a -> a
volumeOfTetrahedron edge = edge^3 * sqrt 2 / 12

fill_tetrahedron = fromCubicCmToL.volumeOfTetrahedron