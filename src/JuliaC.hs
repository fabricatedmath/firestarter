{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JuliaC where

import Control.Monad (replicateM_)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict

import Data.Complex
import Data.Foldable (maximumBy)
import Data.Function (on)

import Firestarter

import Linear

import Numeric.AD

data Julia = Julia (V2 Double) Double

instance Metric Complex

instance Firestarter Julia V2 Double where
  isInside (Julia _ e) v = juliaDistance v <= e
  distanceAndDirection (Julia _ _) v =
    let
      (d,dir) = grad' juliaDistance v
      n = fmap signum $ maximumBy (compare `on` norm) $ map (negate dir*) basis
    in
      (d,n)
  startingLocation _ = V2 2 2

juliaFunction
  :: (Floating a, RealFloat a, Ord a)
  => Complex a
  -> StateT (Complex a,Complex a) (Either (Complex a, Complex a)) ()
juliaFunction c = do
  (q,dq) <- get
  if quadrance q > 200
    then lift $ Left (q,dq)
    else put (q*q+c, 2*^ (q*dq))

juliaDistanceEstimate :: Floating a => (Complex a, Complex a) -> a
juliaDistanceEstimate (q,dq) = 0.5 * r * log r / dr
  where r = norm q
        dr = norm dq

juliaDistance :: (Floating a, Ord a, RealFloat a) => V2 a -> a
juliaDistance v = juliaDistanceEstimate . either id id $ execStateT s qs
  where
    s = replicateM_ 100 $ juliaFunction c
    c = initC
    qs = fromVec v

initC :: Floating a => Complex a
initC = (-0.125) :+ (-0.256)
--initComplex = (-0.4) :+ 0.6

fromVec :: Floating a => V2 a -> (Complex a, Complex a)
fromVec (V2 x y) = (x :+ y, 1 :+ 0)
