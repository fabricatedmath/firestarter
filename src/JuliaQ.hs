{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JuliaQ where

import Control.Monad (replicateM_)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict

import Data.Foldable (maximumBy)
import Data.Function (on)

import Linear

import Numeric.AD

import Firestarter

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
