import Data.List

fill_tetrahedron = fromCubicCmToL.volumeOfTetrahedron
    where
        fromCubicCmToL :: Fractional a => a -> a
        fromCubicCmToL n = n / 1000
        volumeOfTetrahedron :: Floating a => a -> a
        volumeOfTetrahedron edge = edge^3 * sqrt 2 / 12

step :: (Ord a, Num a) => a -> [a] -> Bool
step water tetrahedrons = sum tetrahedrons <= water

tetrahedron_filled tetrahedrons water = helper inDescending water
    where
        inDescending = reverse $ volumeOfTetrahedrons tetrahedrons
        volumeOfTetrahedrons edges = sort $ map fill_tetrahedron $ map fromIntegral edges
        helper [] _ = 0
        helper (x : xs) water
            | step water (x : xs) = length (x : xs)
            | otherwise = helper xs water