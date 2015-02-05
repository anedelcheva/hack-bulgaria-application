-- the function fromCubicCmToL converts cubic metres into litres
fromCubicCmToL :: Fractional a => a -> a
fromCubicCmToL n = n / 1000

-- the function volumeOfTetrahedron finds the volume of a tetrahedron by given edge
volumeOfTetrahedron :: Floating a => a -> a
volumeOfTetrahedron edge = edge^3 * sqrt 2 / 12

-- the function fill_tetrahedron returns the amount of water that can be filled in the tetrahedron
--solution with the function composition (.)
fill_tetrahedron = fromCubicCmToL.volumeOfTetrahedron

