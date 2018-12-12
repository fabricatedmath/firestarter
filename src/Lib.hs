{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Lib (stepFire) where

import Control.DeepSeq

import Data.Bits
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word

import qualified Data.Array.Repa as R

import Linear

import JuliaQ
import Firestarter
import MarchingCubes

import System.IO

class Corners f where
  cornersWithMask :: ([f],Word8)
  corners :: [f]
  corners = fst $ cornersWithMask

instance Num a => Corners (V2 a) where
  cornersWithMask = (V2 <$> [1,-1] <*> [1,-1], 15)

instance Corners (V3 Int) where
  cornersWithMask = (vs,255)
    where vs = --coordinates from Paul Bourke Marching Cubes
            [ V3 1 (-1) 1
            , V3 (-1) (-1) 1
            , V3 (-1) 1 1
            , V3 1 1 1
            , V3 1 (-1) (-1)
            , V3 (-1) (-1) (-1)
            , V3 (-1) 1 (-1)
            , V3 1 1 (-1)
            ]

data B = B !Word8 !Word8 !Word8
       deriving Show

instance NFData B where
  rnf (B a b c) = a `seq` b `seq` c `seq` ()

ttl :: Word8
ttl = 2

toGrid :: (Functor f, RealFrac a) => a -> f a -> f Int
toGrid e = fmap (round . (*(2*recip e)))

fromGrid :: (Functor f, Fractional a) => a -> f Int -> f a
fromGrid e = fmap ((*(0.5*e)) . fromIntegral)

cornerToCenters
  :: (Corners (f a), Num a, Num (f a))
  => (Bool,f a)
  -> [(f a, B)]
cornerToCenters (loc,p) = zip (map (p+) corners) corner
  where
    corner | loc = map ((\x -> B x x ttl) . bit) [0..]
           | otherwise = map ((\x -> B 0 x ttl) . bit) [0..]

centerToCorners
  :: (Corners (f a), Num a, Num (f a))
  => (f a, B)
  -> [f a]
centerToCorners (c,B a b _)
  | a == b || a == 0 = []
  | otherwise = map snd $ filter (testBit b' . fst) $ zip [0..] $ map (c-) cs
  where
    (cs,mask) = cornersWithMask
    b' = complement $ b .&. mask

type Triangle a = V3 (V3 a)

stepFire
  :: Julia
  -> Double
  -> [V3 Double]
  -> (U.Vector (Triangle Double) -> IO ())
  -> IO ()
stepFire j e ips reporter = go (U.fromList $ map (toGrid e) ips) M.empty
  where
    (_cs,mask) = cornersWithMask :: ([V3 Int],Word8)

    mergeCenters :: B -> B -> B
    mergeCenters (B a1 b1 _) (B a2 b2 _) = B (a1 .|. a2) (b1 .|. b2) ttl

    squareFilter :: B -> Bool
    squareFilter (B _ b t) = b /= mask && t /= 0

    decrementTTL :: B -> B
    decrementTTL (B a b t) = B a b (t-1)

    go :: U.Vector (V3 Int) -> M.Map (V3 Int) B -> IO ()
    go ps m =
      do
        points' <-
          fmap (concatMap cornerToCenters . R.toList) $ R.computeUnboxedP $
          R.map
          ( fmap (toGrid e)
          . (\p -> (isInside j p,p)) . fromGrid e
          ) $ toRepa ps
        let
          (m', out) =
            M.partition squareFilter $ M.unionWith mergeCenters m $
            M.fromListWith mergeCenters $ points'
          ps' =
            U.fromList $ S.toList $ S.fromList $
            concatMap centerToCorners $ M.toList m'
          toCubeCases (v, B b _ _) = map (\c -> (v,c,b)) $ cubeCasesLookup $ fromIntegral b
          m'' = M.map decrementTTL m'
        closed' <-
          fmap R.toUnboxed $ R.computeUnboxedP $
          R.map (lookupEdges e juliaDistance) $
          toRepa $ U.fromList $ concatMap toCubeCases $
          M.toList $ M.filter (\(B _ b _) -> b == mask) out
        closed' `deepseq` ps' `deepseq` m'' `deepseq` return ()
        reporter closed'
        hPutStrLn stderr $ show $ U.length ps
        case U.null ps' of
          True -> return ()
          False -> go ps' m''

toRepa :: U.Unbox a => U.Vector a -> R.Array R.U R.DIM1 a
toRepa v = R.fromUnboxed (R.Z R.:. U.length v) v

lookupEdges
  :: Double
  -> (V3 Double -> Double)
  -> (V3 Int, V3 Int, Word8)
  -> Triangle Double
lookupEdges isolevel oracle (v, p, b) =
  fmap (\i -> lookupEdge isolevel oracle (v, i, b)) p

lookupEdge
  :: Double
  -> (V3 Double -> Double)
  -> (V3 Int, Int, Word8)
  -> V3 Double
lookupEdge isolevel oracle (v,c,b) =
  let
    f (x,y) | testBit b x = (y,x)
            | testBit b y = (x,y)
            | otherwise = error "dogs"
    (b1,b2) = f $ edgePointsLookup c
    p1 = fromGrid isolevel $ v - from b1
    p2 = fromGrid isolevel $ v - from b2
  in findPoint isolevel oracle p1 p2

findPoint :: Double -> Oracle Double -> V3 Double -> V3 Double -> V3 Double
findPoint isolevel oracle p1 p2 = snd . last . take 20 $ iterate (subdivide isolevel oracle dir) (p2,p1)
  where dir = normalize $ p2 - p1
{-# INLINE findPoint #-}

subdivide :: Double -> Oracle Double -> V3 Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
subdivide isolevel oracle dir (p1,p2)
  | dist < 0 = (mid,p2)
  | otherwise = (p1, mid + dir ^* dist)
  where mid = (p1 + p2) ^* 0.5
        dist = oracle mid - isolevel
{-# INLINE subdivide #-}

from' :: U.Vector (V3 Int)
from' = U.fromList corners

from :: Int -> V3 Int
from i = from' U.! i
