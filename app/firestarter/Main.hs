{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Lib
import Firestarter
import JuliaC

import Linear
import Control.Arrow ((***))

import qualified Data.Map.Strict as M
import Data.Word (Word8)

import Graphics.Gloss.Interface.IO.Simulate hiding (Point)

pointToPic :: (Fractional a, Real a) => a -> Point V2 a -> Picture
pointToPic e (Point inside v) =
  let
    (V2 x y) = v
    pic
      | inside = circleSolid $ realToFrac $ 0.5 * e
      | otherwise = circle $ realToFrac $ 0.5 * e
  in
    translate (realToFrac $ x) (realToFrac $ y) pic

modelToPic :: Float -> Model -> IO Picture
modelToPic s (Model _ e m points closed) =
  let
    newPoints = map (pointToPic e) points
    closedPoints = map (color red . pointToPic e) closed
    setPoints = map (color green . pointToPic e . fromGrid e) $
                concatMap centerToCornerPoints $ M.toList m
  in
    return $ scale s s $ pictures $ closedPoints ++ setPoints ++ newPoints

modelState :: ViewPort -> Float -> Model -> IO Model
modelState _ _ (Model j e m ps cs) =
  let
    (closed,points,m') = stepFire e ps m
    points' = map (\p -> Point (isInside j p) p) points
    closed' = map (fromGrid e) $ concatMap centerToCornerPoints closed
  in
    return $ Model j e m' points' (cs ++ closed')

data Model =
  Model
  { _firestarter :: Julia
  , _gridSize :: Double
  , _map :: M.Map (V2 Int) B
  , _points :: [Point V2 Double]
  , _closed :: [Point V2 Double]
  }

main :: IO ()
main =
  do
    let
      gridSize = 0.005
      j = Julia (V2 2 2) gridSize
      (p1,p2) = (Point False *** Point True) $ lastTwoOnGrid j gridSize
      window = (InWindow "dogs" (2560,1440) (0,0))
      initialModel = Model j gridSize M.empty [p1,p2] []
    simulateIO window white 100 initialModel (modelToPic 600) modelState
