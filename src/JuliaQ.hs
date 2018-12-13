{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JuliaQ where

import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict

import Data.Foldable (maximumBy)
import Data.Function (on)

import Linear

import Numeric.AD

import Firestarter

type Q = Quaternion

data Julia = Julia (V3 Double) Double
           deriving Show

instance Firestarter Julia V3 Double where
  isInside (Julia _ e) v = juliaDistance v <= e
  distanceAndDirection (Julia _ _) v =
    let
      (d,dir) = grad' juliaDistance v
      n = fmap signum $ maximumBy (compare `on` norm) $ map (negate dir*) basis
    in
      (d,n)
  startingLocation _ = V3 2 2 2

instance HasNormal Julia V3 Double where
  normal _ = juliaAnalyticNormal 100 200 initC

juliaFunction
  :: (Floating a, RealFloat a, Num a, Ord a)
  => Quaternion a
  -> StateT (Quaternion a, Quaternion a) (Either (Quaternion a, Quaternion a)) ()
juliaFunction c = do
  (q,dq) <- get
  if quadrance q > 200
    then lift $ Left (q,dq)
    else put (q*q+c, 2*^(q*dq))

juliaDistanceEstimate :: Floating a => (Quaternion a, Quaternion a) -> a
juliaDistanceEstimate (q,dq) = 0.5 * r * log r / dr
  where r = norm q
        dr = norm dq

juliaDistance :: (Floating a, Ord a, RealFloat a) => V3 a -> a
juliaDistance v = juliaDistanceEstimate . either id id $ execStateT s qs
  where s = replicateM_ 100 $ juliaFunction c
        c = initC
        qs = fromVec v

initC :: Floating a => Quaternion a
initC = Quaternion (-0.125) $ V3 (-0.256) 0.847 0

fromVec :: Floating a => V3 a -> (Quaternion a, Quaternion a)
fromVec (V3 x y z) = (Quaternion x $ V3 y z 0, Quaternion 1 zero)

-- | Analytical Normal of Julia set (z^2 + c) over the Quaternions
--
-- Full credit goes to Inigo Quilez for derivation and original code at
--
-- https://www.shadertoy.com/view/MsfGRr
juliaAnalyticNormal
  :: forall a. (Epsilon a, RealFloat a)
  => Int
  -> a
  -> Quaternion a
  -> V3 a
  -> V3 a
juliaAnalyticNormal iters bailout c v =
  let
    iz = (Quaternion x' $ V3 y' z' 0)
      where (V3 x' y' z') = v
    idz0 = _e .~ 1 $ 0
    idz1 = _i .~ 1 $ 0
    idz2 = _j .~ 1 $ 0
    idz3 = _k .~ 1 $ 0

    f :: Int -> (Q a, Q a, Q a, Q a, Q a) -> V3 a
    f i (!z,!dz0,!dz1,!dz2,!dz3)
      | i == 0 || quadrance z > bailout =
          normalize $ V3 (z `dot` dz0) (z `dot` dz1) (z `dot` dz2)
      | otherwise =
        let
          mz = Quaternion (z ^. _e) $ negate $ z ^. _ijk
          dzmul dz =
            Quaternion (mz `dot` dz) $
            ((z ^. _e) *^ (dz ^. _ijk) + (dz ^. _e) *^ (z ^. _ijk))
          dz0' = dzmul dz0
          dz1' = dzmul dz1
          dz2' = dzmul dz2
          dz3' = dzmul dz3
          z' = z*z + c
        in f (i-1) (z',dz0',dz1',dz2',dz3')
  in
    f iters (iz,idz0,idz1,idz2,idz3)
{-# INLINABLE juliaAnalyticNormal #-}
