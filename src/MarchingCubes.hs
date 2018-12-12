module MarchingCubes (Oracle, cubeCasesLookup, edgePointsLookup) where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Linear

type Oracle a = V3 a -> a

edgePointsLookup :: Int -> (Int,Int)
edgePointsLookup = (edgePoints V.!)
{-# INLINE edgePointsLookup #-}

edgePoints :: Vector (Int,Int)
edgePoints =
  V.fromList
  [ (0,1), (1,2), (2,3), (3,0)
  , (4,5), (5,6), (6,7), (7,4)
  , (0,4), (1,5), (2,6), (3,7)
  ]
{-# NOINLINE edgePoints #-}

cubeCasesLookup :: Int -> [V3 Int]
cubeCasesLookup = (cubeCases V.!)
{-# INLINE cubeCasesLookup #-}

{-# NOINLINE cubeCases #-}
cubeCases :: Vector [V3 Int]
cubeCases = V.fromList
  [ []
  , [V3 0 8 3]
  , [V3 0 1 9]
  , [V3 1 8 3, V3 9 8 1]
  , [V3 1 2 10]
  , [V3 0 8 3, V3 1 2 10]
  , [V3 9 2 10, V3 0 2 9]
  , [V3 2 8 3, V3 2 10 8, V3 10 9 8]
  , [V3 3 11 2]
  , [V3 0 11 2, V3 8 11 0]
  , [V3 1 9 0, V3 2 3 11]
  , [V3 1 11 2, V3 1 9 11, V3 9 8 11]
  , [V3 3 10 1, V3 11 10 3]
  , [V3 0 10 1, V3 0 8 10, V3 8 11 10]
  , [V3 3 9 0, V3 3 11 9, V3 11 10 9]
  , [V3 9 8 10, V3 10 8 11]
  , [V3 4 7 8]
  , [V3 4 3 0, V3 7 3 4]
  , [V3 0 1 9, V3 8 4 7]
  , [V3 4 1 9, V3 4 7 1, V3 7 3 1]
  , [V3 1 2 10, V3 8 4 7]
  , [V3 3 4 7, V3 3 0 4, V3 1 2 10]
  , [V3 9 2 10, V3 9 0 2, V3 8 4 7]
  , [V3 2 10 9, V3 2 9 7, V3 2 7 3, V3 7 9 4]
  , [V3 8 4 7, V3 3 11 2]
  , [V3 11 4 7, V3 11 2 4, V3 2 0 4]
  , [V3 9 0 1, V3 8 4 7, V3 2 3 11]
  , [V3 4 7 11, V3 9 4 11, V3 9 11 2, V3 9 2 1]
  , [V3 3 10 1, V3 3 11 10, V3 7 8 4]
  , [V3 1 11 10, V3 1 4 11, V3 1 0 4, V3 7 11 4]
  , [V3 4 7 8, V3 9 0 11, V3 9 11 10, V3 11 0 3]
  , [V3 4 7 11, V3 4 11 9, V3 9 11 10]
  , [V3 9 5 4]
  , [V3 9 5 4, V3 0 8 3]
  , [V3 0 5 4, V3 1 5 0]
  , [V3 8 5 4, V3 8 3 5, V3 3 1 5]
  , [V3 1 2 10, V3 9 5 4]
  , [V3 3 0 8, V3 1 2 10, V3 4 9 5]
  , [V3 5 2 10, V3 5 4 2, V3 4 0 2]
  , [V3 2 10 5, V3 3 2 5, V3 3 5 4, V3 3 4 8]
  , [V3 9 5 4, V3 2 3 11]
  , [V3 0 11 2, V3 0 8 11, V3 4 9 5]
  , [V3 0 5 4, V3 0 1 5, V3 2 3 11]
  , [V3 2 1 5, V3 2 5 8, V3 2 8 11, V3 4 8 5]
  , [V3 10 3 11, V3 10 1 3, V3 9 5 4]
  , [V3 4 9 5, V3 0 8 1, V3 8 10 1, V3 8 11 10]
  , [V3 5 4 0, V3 5 0 11, V3 5 11 10, V3 11 0 3]
  , [V3 5 4 8, V3 5 8 10, V3 10 8 11]
  , [V3 9 7 8, V3 5 7 9]
  , [V3 9 3 0, V3 9 5 3, V3 5 7 3]
  , [V3 0 7 8, V3 0 1 7, V3 1 5 7]
  , [V3 1 5 3, V3 3 5 7]
  , [V3 9 7 8, V3 9 5 7, V3 10 1 2]
  , [V3 10 1 2, V3 9 5 0, V3 5 3 0, V3 5 7 3]
  , [V3 8 0 2, V3 8 2 5, V3 8 5 7, V3 10 5 2]
  , [V3 2 10 5, V3 2 5 3, V3 3 5 7]
  , [V3 7 9 5, V3 7 8 9, V3 3 11 2]
  , [V3 9 5 7, V3 9 7 2, V3 9 2 0, V3 2 7 11]
  , [V3 2 3 11, V3 0 1 8, V3 1 7 8, V3 1 5 7]
  , [V3 11 2 1, V3 11 1 7, V3 7 1 5]
  , [V3 9 5 8, V3 8 5 7, V3 10 1 3, V3 10 3 11]
  , [V3 5 7 0, V3 5 0 9, V3 7 11 0, V3 1 0 10, V3 11 10 0]
  , [V3 11 10 0, V3 11 0 3, V3 10 5 0, V3 8 0 7, V3 5 7 0]
  , [V3 11 10 5, V3 7 11 5]
  , [V3 10 6 5]
  , [V3 0 8 3, V3 5 10 6]
  , [V3 9 0 1, V3 5 10 6]
  , [V3 1 8 3, V3 1 9 8, V3 5 10 6]
  , [V3 1 6 5, V3 2 6 1]
  , [V3 1 6 5, V3 1 2 6, V3 3 0 8]
  , [V3 9 6 5, V3 9 0 6, V3 0 2 6]
  , [V3 5 9 8, V3 5 8 2, V3 5 2 6, V3 3 2 8]
  , [V3 2 3 11, V3 10 6 5]
  , [V3 11 0 8, V3 11 2 0, V3 10 6 5]
  , [V3 0 1 9, V3 2 3 11, V3 5 10 6]
  , [V3 5 10 6, V3 1 9 2, V3 9 11 2, V3 9 8 11]
  , [V3 6 3 11, V3 6 5 3, V3 5 1 3]
  , [V3 0 8 11, V3 0 11 5, V3 0 5 1, V3 5 11 6]
  , [V3 3 11 6, V3 0 3 6, V3 0 6 5, V3 0 5 9]
  , [V3 6 5 9, V3 6 9 11, V3 11 9 8]
  , [V3 5 10 6, V3 4 7 8]
  , [V3 4 3 0, V3 4 7 3, V3 6 5 10]
  , [V3 1 9 0, V3 5 10 6, V3 8 4 7]
  , [V3 10 6 5, V3 1 9 7, V3 1 7 3, V3 7 9 4]
  , [V3 6 1 2, V3 6 5 1, V3 4 7 8]
  , [V3 1 2 5, V3 5 2 6, V3 3 0 4, V3 3 4 7]
  , [V3 8 4 7, V3 9 0 5, V3 0 6 5, V3 0 2 6]
  , [V3 7 3 9, V3 7 9 4, V3 3 2 9, V3 5 9 6, V3 2 6 9]
  , [V3 3 11 2, V3 7 8 4, V3 10 6 5]
  , [V3 5 10 6, V3 4 7 2, V3 4 2 0, V3 2 7 11]
  , [V3 0 1 9, V3 4 7 8, V3 2 3 11, V3 5 10 6]
  , [V3 9 2 1, V3 9 11 2, V3 9 4 11, V3 7 11 4, V3 5 10 6]
  , [V3 8 4 7, V3 3 11 5, V3 3 5 1, V3 5 11 6]
  , [V3 5 1 11, V3 5 11 6, V3 1 0 11, V3 7 11 4, V3 0 4 11]
  , [V3 0 5 9, V3 0 6 5, V3 0 3 6, V3 11 6 3, V3 8 4 7]
  , [V3 6 5 9, V3 6 9 11, V3 4 7 9, V3 7 11 9]
  , [V3 10 4 9, V3 6 4 10]
  , [V3 4 10 6, V3 4 9 10, V3 0 8 3]
  , [V3 10 0 1, V3 10 6 0, V3 6 4 0]
  , [V3 8 3 1, V3 8 1 6, V3 8 6 4, V3 6 1 10]
  , [V3 1 4 9, V3 1 2 4, V3 2 6 4]
  , [V3 3 0 8, V3 1 2 9, V3 2 4 9, V3 2 6 4]
  , [V3 0 2 4, V3 4 2 6]
  , [V3 8 3 2, V3 8 2 4, V3 4 2 6]
  , [V3 10 4 9, V3 10 6 4, V3 11 2 3]
  , [V3 0 8 2, V3 2 8 11, V3 4 9 10, V3 4 10 6]
  , [V3 3 11 2, V3 0 1 6, V3 0 6 4, V3 6 1 10]
  , [V3 6 4 1, V3 6 1 10, V3 4 8 1, V3 2 1 11, V3 8 11 1]
  , [V3 9 6 4, V3 9 3 6, V3 9 1 3, V3 11 6 3]
  , [V3 8 11 1, V3 8 1 0, V3 11 6 1, V3 9 1 4, V3 6 4 1]
  , [V3 3 11 6, V3 3 6 0, V3 0 6 4]
  , [V3 6 4 8, V3 11 6 8]
  , [V3 7 10 6, V3 7 8 10, V3 8 9 10]
  , [V3 0 7 3, V3 0 10 7, V3 0 9 10, V3 6 7 10]
  , [V3 10 6 7, V3 1 10 7, V3 1 7 8, V3 1 8 0]
  , [V3 10 6 7, V3 10 7 1, V3 1 7 3]
  , [V3 1 2 6, V3 1 6 8, V3 1 8 9, V3 8 6 7]
  , [V3 2 6 9, V3 2 9 1, V3 6 7 9, V3 0 9 3, V3 7 3 9]
  , [V3 7 8 0, V3 7 0 6, V3 6 0 2]
  , [V3 7 3 2, V3 6 7 2]
  , [V3 2 3 11, V3 10 6 8, V3 10 8 9, V3 8 6 7]
  , [V3 2 0 7, V3 2 7 11, V3 0 9 7, V3 6 7 10, V3 9 10 7]
  , [V3 1 8 0, V3 1 7 8, V3 1 10 7, V3 6 7 10, V3 2 3 11]
  , [V3 11 2 1, V3 11 1 7, V3 10 6 1, V3 6 7 1]
  , [V3 8 9 6, V3 8 6 7, V3 9 1 6, V3 11 6 3, V3 1 3 6]
  , [V3 0 9 1, V3 11 6 7]
  , [V3 7 8 0, V3 7 0 6, V3 3 11 0, V3 11 6 0]
  , [V3 7 11 6]
  , [V3 7 6 11]
  , [V3 3 0 8, V3 11 7 6]
  , [V3 0 1 9, V3 11 7 6]
  , [V3 8 1 9, V3 8 3 1, V3 11 7 6]
  , [V3 10 1 2, V3 6 11 7]
  , [V3 1 2 10, V3 3 0 8, V3 6 11 7]
  , [V3 2 9 0, V3 2 10 9, V3 6 11 7]
  , [V3 6 11 7, V3 2 10 3, V3 10 8 3, V3 10 9 8]
  , [V3 7 2 3, V3 6 2 7]
  , [V3 7 0 8, V3 7 6 0, V3 6 2 0]
  , [V3 2 7 6, V3 2 3 7, V3 0 1 9]
  , [V3 1 6 2, V3 1 8 6, V3 1 9 8, V3 8 7 6]
  , [V3 10 7 6, V3 10 1 7, V3 1 3 7]
  , [V3 10 7 6, V3 1 7 10, V3 1 8 7, V3 1 0 8]
  , [V3 0 3 7, V3 0 7 10, V3 0 10 9, V3 6 10 7]
  , [V3 7 6 10, V3 7 10 8, V3 8 10 9]
  , [V3 6 8 4, V3 11 8 6]
  , [V3 3 6 11, V3 3 0 6, V3 0 4 6]
  , [V3 8 6 11, V3 8 4 6, V3 9 0 1]
  , [V3 9 4 6, V3 9 6 3, V3 9 3 1, V3 11 3 6]
  , [V3 6 8 4, V3 6 11 8, V3 2 10 1]
  , [V3 1 2 10, V3 3 0 11, V3 0 6 11, V3 0 4 6]
  , [V3 4 11 8, V3 4 6 11, V3 0 2 9, V3 2 10 9]
  , [V3 10 9 3, V3 10 3 2, V3 9 4 3, V3 11 3 6, V3 4 6 3]
  , [V3 8 2 3, V3 8 4 2, V3 4 6 2]
  , [V3 0 4 2, V3 4 6 2]
  , [V3 1 9 0, V3 2 3 4, V3 2 4 6, V3 4 3 8]
  , [V3 1 9 4, V3 1 4 2, V3 2 4 6]
  , [V3 8 1 3, V3 8 6 1, V3 8 4 6, V3 6 10 1]
  , [V3 10 1 0, V3 10 0 6, V3 6 0 4]
  , [V3 4 6 3, V3 4 3 8, V3 6 10 3, V3 0 3 9, V3 10 9 3]
  , [V3 10 9 4, V3 6 10 4]
  , [V3 4 9 5, V3 7 6 11]
  , [V3 0 8 3, V3 4 9 5, V3 11 7 6]
  , [V3 5 0 1, V3 5 4 0, V3 7 6 11]
  , [V3 11 7 6, V3 8 3 4, V3 3 5 4, V3 3 1 5]
  , [V3 9 5 4, V3 10 1 2, V3 7 6 11]
  , [V3 6 11 7, V3 1 2 10, V3 0 8 3, V3 4 9 5]
  , [V3 7 6 11, V3 5 4 10, V3 4 2 10, V3 4 0 2]
  , [V3 3 4 8, V3 3 5 4, V3 3 2 5, V3 10 5 2, V3 11 7 6]
  , [V3 7 2 3, V3 7 6 2, V3 5 4 9]
  , [V3 9 5 4, V3 0 8 6, V3 0 6 2, V3 6 8 7]
  , [V3 3 6 2, V3 3 7 6, V3 1 5 0, V3 5 4 0]
  , [V3 6 2 8, V3 6 8 7, V3 2 1 8, V3 4 8 5, V3 1 5 8]
  , [V3 9 5 4, V3 10 1 6, V3 1 7 6, V3 1 3 7]
  , [V3 1 6 10, V3 1 7 6, V3 1 0 7, V3 8 7 0, V3 9 5 4]
  , [V3 4 0 10, V3 4 10 5, V3 0 3 10, V3 6 10 7, V3 3 7 10]
  , [V3 7 6 10, V3 7 10 8, V3 5 4 10, V3 4 8 10]
  , [V3 6 9 5, V3 6 11 9, V3 11 8 9]
  , [V3 3 6 11, V3 0 6 3, V3 0 5 6, V3 0 9 5]
  , [V3 0 11 8, V3 0 5 11, V3 0 1 5, V3 5 6 11]
  , [V3 6 11 3, V3 6 3 5, V3 5 3 1]
  , [V3 1 2 10, V3 9 5 11, V3 9 11 8, V3 11 5 6]
  , [V3 0 11 3, V3 0 6 11, V3 0 9 6, V3 5 6 9, V3 1 2 10]
  , [V3 11 8 5, V3 11 5 6, V3 8 0 5, V3 10 5 2, V3 0 2 5]
  , [V3 6 11 3, V3 6 3 5, V3 2 10 3, V3 10 5 3]
  , [V3 5 8 9, V3 5 2 8, V3 5 6 2, V3 3 8 2]
  , [V3 9 5 6, V3 9 6 0, V3 0 6 2]
  , [V3 1 5 8, V3 1 8 0, V3 5 6 8, V3 3 8 2, V3 6 2 8]
  , [V3 1 5 6, V3 2 1 6]
  , [V3 1 3 6, V3 1 6 10, V3 3 8 6, V3 5 6 9, V3 8 9 6]
  , [V3 10 1 0, V3 10 0 6, V3 9 5 0, V3 5 6 0]
  , [V3 0 3 8, V3 5 6 10]
  , [V3 10 5 6]
  , [V3 11 5 10, V3 7 5 11]
  , [V3 11 5 10, V3 11 7 5, V3 8 3 0]
  , [V3 5 11 7, V3 5 10 11, V3 1 9 0]
  , [V3 10 7 5, V3 10 11 7, V3 9 8 1, V3 8 3 1]
  , [V3 11 1 2, V3 11 7 1, V3 7 5 1]
  , [V3 0 8 3, V3 1 2 7, V3 1 7 5, V3 7 2 11]
  , [V3 9 7 5, V3 9 2 7, V3 9 0 2, V3 2 11 7]
  , [V3 7 5 2, V3 7 2 11, V3 5 9 2, V3 3 2 8, V3 9 8 2]
  , [V3 2 5 10, V3 2 3 5, V3 3 7 5]
  , [V3 8 2 0, V3 8 5 2, V3 8 7 5, V3 10 2 5]
  , [V3 9 0 1, V3 5 10 3, V3 5 3 7, V3 3 10 2]
  , [V3 9 8 2, V3 9 2 1, V3 8 7 2, V3 10 2 5, V3 7 5 2]
  , [V3 1 3 5, V3 3 7 5]
  , [V3 0 8 7, V3 0 7 1, V3 1 7 5]
  , [V3 9 0 3, V3 9 3 5, V3 5 3 7]
  , [V3 9 8 7, V3 5 9 7]
  , [V3 5 8 4, V3 5 10 8, V3 10 11 8]
  , [V3 5 0 4, V3 5 11 0, V3 5 10 11, V3 11 3 0]
  , [V3 0 1 9, V3 8 4 10, V3 8 10 11, V3 10 4 5]
  , [V3 10 11 4, V3 10 4 5, V3 11 3 4, V3 9 4 1, V3 3 1 4]
  , [V3 2 5 1, V3 2 8 5, V3 2 11 8, V3 4 5 8]
  , [V3 0 4 11, V3 0 11 3, V3 4 5 11, V3 2 11 1, V3 5 1 11]
  , [V3 0 2 5, V3 0 5 9, V3 2 11 5, V3 4 5 8, V3 11 8 5]
  , [V3 9 4 5, V3 2 11 3]
  , [V3 2 5 10, V3 3 5 2, V3 3 4 5, V3 3 8 4]
  , [V3 5 10 2, V3 5 2 4, V3 4 2 0]
  , [V3 3 10 2, V3 3 5 10, V3 3 8 5, V3 4 5 8, V3 0 1 9]
  , [V3 5 10 2, V3 5 2 4, V3 1 9 2, V3 9 4 2]
  , [V3 8 4 5, V3 8 5 3, V3 3 5 1]
  , [V3 0 4 5, V3 1 0 5]
  , [V3 8 4 5, V3 8 5 3, V3 9 0 5, V3 0 3 5]
  , [V3 9 4 5]
  , [V3 4 11 7, V3 4 9 11, V3 9 10 11]
  , [V3 0 8 3, V3 4 9 7, V3 9 11 7, V3 9 10 11]
  , [V3 1 10 11, V3 1 11 4, V3 1 4 0, V3 7 4 11]
  , [V3 3 1 4, V3 3 4 8, V3 1 10 4, V3 7 4 11, V3 10 11 4]
  , [V3 4 11 7, V3 9 11 4, V3 9 2 11, V3 9 1 2]
  , [V3 9 7 4, V3 9 11 7, V3 9 1 11, V3 2 11 1, V3 0 8 3]
  , [V3 11 7 4, V3 11 4 2, V3 2 4 0]
  , [V3 11 7 4, V3 11 4 2, V3 8 3 4, V3 3 2 4]
  , [V3 2 9 10, V3 2 7 9, V3 2 3 7, V3 7 4 9]
  , [V3 9 10 7, V3 9 7 4, V3 10 2 7, V3 8 7 0, V3 2 0 7]
  , [V3 3 7 10, V3 3 10 2, V3 7 4 10, V3 1 10 0, V3 4 0 10]
  , [V3 1 10 2, V3 8 7 4]
  , [V3 4 9 1, V3 4 1 7, V3 7 1 3]
  , [V3 4 9 1, V3 4 1 7, V3 0 8 1, V3 8 7 1]
  , [V3 4 0 3, V3 7 4 3]
  , [V3 4 8 7]
  , [V3 9 10 8, V3 10 11 8]
  , [V3 3 0 9, V3 3 9 11, V3 11 9 10]
  , [V3 0 1 10, V3 0 10 8, V3 8 10 11]
  , [V3 3 1 10, V3 11 3 10]
  , [V3 1 2 11, V3 1 11 9, V3 9 11 8]
  , [V3 3 0 9, V3 3 9 11, V3 1 2 9, V3 2 11 9]
  , [V3 0 2 11, V3 8 0 11]
  , [V3 3 2 11]
  , [V3 2 3 8, V3 2 8 10, V3 10 8 9]
  , [V3 9 10 2, V3 0 9 2]
  , [V3 2 3 8, V3 2 8 10, V3 0 1 8, V3 1 10 8]
  , [V3 1 10 2]
  , [V3 1 3 8, V3 9 1 8]
  , [V3 0 9 1]
  , [V3 0 3 8]
  , []
  ]
