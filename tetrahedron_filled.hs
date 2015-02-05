-- we import Data.List in order to use the function sort
import Data.List

-- the function fill_tetrahedron returns the amount of water that can be filled in the tetrahedron
fill_tetrahedron = fromCubicCmToL.volumeOfTetrahedron
    where
        -- the function fromCubicCmToL converts cubic metres into litres
        fromCubicCmToL :: Fractional a => a -> a
        fromCubicCmToL n = n / 1000
        -- the function volumeOfTetrahedron finds the volume of a tetrahedron by given edge
        volumeOfTetrahedron :: Floating a => a -> a
        volumeOfTetrahedron edge = edge^3 * sqrt 2 / 12

{-
 1) Applying map to the list of edges with the function fromIntegral with declaration: fromIntegral :: (Num b, Integral a) => a -> b
 2) Applying map to the list of edges with the function fill_tetrahedron (we get a list of the volumes of the tetrahedrons)
 3) We sort the list of volumes in ascending order -}
volumeOfTetrahedrons edges = sort $ map fill_tetrahedron $ map fromIntegral edges

-- the funcion step checks whether the sum of volumes of tetrahedrons is less than or equal to the amount of water
step :: (Ord a, Num a) => a -> [a] -> Bool
step water tetrahedrons = sum tetrahedrons <= water

{-
 we use a function helper which takes the parameter:
   *) inDescending - the volumes of tetrahedrons sorted in descending order
  **) water - the amount of water in litres
 Algorithm: 
 If the sum of volumes of tetrahedrons is less than water we get the length of the list, 
 otherwise we call helper without the head of the list (i. e. we remove the tetrahedron with biggest volume)
-}
tetrahedron_filled tetrahedrons water = helper inDescending water
    where
        inDescending = reverse $ volumeOfTetrahedrons tetrahedrons
        helper [] _ = 0
        helper (x : xs) water
            | step water (x : xs) = length (x : xs)
            | otherwise = helper xs water

list = [100,20, 30]
list2 = [100, 20, 30, 50, 10]