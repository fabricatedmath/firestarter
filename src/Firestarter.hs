{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Firestarter where

import Data.Word

import Linear

class Firestarter o f a | o -> f, o -> a where
  isInside :: o -> f a -> Bool
  distanceAndDirection :: o -> f a -> (a, f a)
  startingLocation :: o -> f a

lastTwoOnGrid
  :: (Firestarter o f a, Functor f, Num a, Num (f a))
  => o
  -> a
  -> (f a, f a)
lastTwoOnGrid o cl = go $ startingLocation o
  where
    go v
      | isInside o v' = (v,v')
      | otherwise = go v'
      where
        (_d,n) = distanceAndDirection o v
        v' = v + fmap (cl *) n
