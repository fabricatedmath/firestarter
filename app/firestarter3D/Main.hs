module Main where

import Control.Arrow ((***))

import qualified Data.HashMap.Strict as M
import Data.Word

import Linear

import Vis

import Lib
import Firestarter
import JuliaQ

pointToPic :: (Fractional a, Real a) => Color -> a -> Point V3 a -> VisObject a
pointToPic color e (Point inside v) =
  let
    pic
      | inside = Sphere (0.5*e) Solid color
      | otherwise = Sphere (0.5*e) Wireframe color
  in
    Trans v pic

modelToPic :: Double -> Model -> IO (VisObject Double)
modelToPic s (Model _ e m points closed) =
  let
    newPoints = map (pointToPic blue e) points
    closedPoints = [] --map (pointToPic red e) closed
    setPoints = map (pointToPic green e . fromGrid e) $
                concatMap centerToCornerPoints $ M.toList m
  --  points = map _point points
  in
--    return $ Scale (s,s,s) $ VisObjects $ closedPoints ++ setPoints ++ newPoints
    return $ Scale (s,s,s) $ Points (map _point points) Nothing red

modelState :: Float -> Model -> IO Model
modelState _ model =
  let
    Model j e m ps cs = model
    (closed,points,m') = stepFire e ps m
    points' = map (\p -> Point (isInside j p) p) points
    closed' = map (fromGrid e) $ concatMap centerToCornerPoints closed
  in
    return $ Model j e m' points' (cs ++ closed')

data Model =
  Model
  { _firestarter :: Julia
  , _gridSize :: Double
  , _map :: M.HashMap (V3 Int) B
  , _points :: [Point V3 Double]
  , _closed :: [Point V3 Double]
  }

main :: IO ()
main =
  let
    gridSize = 0.01
    j = Julia (V3 2 2 2) gridSize
    (p1,p2) = (Point False *** Point True) $ lastTwoOnGrid j gridSize
    initialModel = Model j gridSize M.empty [p1,p2] []
  in
    simulateIO defaultOpts 0.2 initialModel (modelToPic 1) modelState
