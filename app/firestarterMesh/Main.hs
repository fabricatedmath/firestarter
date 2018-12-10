{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Arrow ((***))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.DeepSeq (deepseq)
import Control.Monad

import qualified Data.Array.Repa as R

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import qualified Data.Serialize as S
import qualified Data.Vector.Unboxed as U

import Data.List (partition)

import Data.Word

--import Graphics.Formats.STL
import GHC.Float

import System.IO

import Linear

import System.Environment

import JuliaQ
import Firestarter
import Lib
import MarchingCubes

modelState' :: Julia -> Double -> [(Bool,V3 Double)] -> IO ()
modelState' j e ips = go ips M.empty
  where
    go :: [(Bool, V3 Double)] -> M.Map (V3 Int) B -> IO ()
    go ps m =
          do
            let
              toRepa v = R.fromUnboxed (R.Z R.:. U.length v) v
              (closed,points,m') = stepFire e ps m
            m' `deepseq` points `deepseq` closed `deepseq` return ()
--            mapM_ print $ concatMap (\(v,(B b _ _)) -> lookupTriangles e e juliaDistance v b) closed
            case U.null points of
              True -> return ()
              False ->
                do
                  points' <- fmap R.toList $ R.computeUnboxedP $ R.map ((\p -> (isInside j p,p)) . fromGrid e) $ toRepa points
                  hPutStrLn stderr $ show $ length points'
                  m' `deepseq` points' `deepseq` go points' m'

{-
modelState :: Model -> Model
modelState (Model j e m ps cs) =
  let
    (closed,points,m') = stepFire e ps m
    points' = map (\p -> Point (isInside j p) p) points
    closed' = cs ++ concatMap (\(v,(B b _ _)) -> lookupTriangles e e juliaDistance v b) closed
  in
    m' `deepseq` closed' `deepseq` points' `deepseq` Model j e m' points' closed'
-}

data Model =
  Model
  { _firestarter :: Julia
  , _gridSize :: Double
  , _map :: M.Map (V3 Int) B
  , _points :: [(Bool, V3 Double)]
  , _closed :: [InternalTriangle]
  } deriving Show

main :: IO ()
main =
  do
    do --test
      let filename = "dog.stl"
      h <- openBinaryFile filename ReadWriteMode
      BS.hPutBuilder h $ putHeader
      BS.hPutBuilder h $ putLength 2
      let list = [V3 0 0 0, V3 0 0 1, V3 0 1 0, V3 0 0 0, V3 0 0 (-1), V3 0 (-1) 0]
      BS.hPutBuilder h $ putVector $ U.fromList (list)
      --writeVector h $ U.fromList (list ++ list ++ list ++ list)
      hClose h

    [gridSizeS,outputFile] <- getArgs
    let
      gridSize = read gridSizeS :: Double --0.005
      j = Julia (V3 2 2 2) gridSize
      (p1,p2) = lastTwoOnGrid j gridSize
    chan <- newTChanIO
--      initialModel = Model j gridSize M.empty [p1,p2] []
--      result = head . snd . partition (not . null . _points) $ iterate modelState initialModel
--      tris = _closed result
    let fileName = outputFile ++ show gridSize ++ ".stl"
--    modelState' j gridSize [p1,p2]
--    let tris = []
--    BS.writeFile (outputFile ++ ".stl") . S.encode . STL "dog" . map (Triangle Nothing) $ tris
    doneVar <- newEmptyMVar
    void $ forkIO $
      do
          h <- openBinaryFile fileName ReadWriteMode
          BS.hPutBuilder h $ putHeader
          BS.hPutBuilder h $ putLength 0
          let
            go i =
              do
                mv <- atomically $ readTChan chan
                case mv of
                  Just v ->
                    do
                      print v
                      BS.hPutBuilder h (putVector v) >> let i' = U.length v + i in i' `seq` go i'
                  Nothing -> return i
          i <- go 0
          hSeek h AbsoluteSeek 80
          BS.hPutBuilder h $ putLength i
          hClose h
          putMVar doneVar ()

    stepFire' j gridSize [p1,p2] (atomically . writeTChan chan . Just)
    atomically $ writeTChan chan Nothing
    _done <- readMVar doneVar
    print "done"
    --BS.writeFile (outputFile ++ ".stl") . S.encode . STL "dog" . map (Triangle Nothing) $ package $ U.toList result

data Header = Header

data Length = Length Int

data Triangle = Triangle (V3 Float) (V3 Float) (V3 Float)

instance S.Serialize Header where
  get = S.skip 80 >> return Header
  put _ = S.put $ BS.replicate 80 (0 :: Word8)

instance S.Serialize Length where
  get = Length . fromIntegral <$> S.getWord32le
  put (Length i) = S.putWord32le $ fromIntegral i

instance S.Serialize Triangle where
  get =
    do
      _n <- S.get :: S.Get (V3 Float)
      v1 <- S.get
      v2 <- S.get
      v3 <- S.get
      S.skip 2
      return $ Triangle v1 v2 v3
  put (Triangle v1 v2 v3) =
    do
      mapM_ S.putFloat32le $ (V3 0 0 0 :: V3 Float)
      mapM_ S.putFloat32le v1
      mapM_ S.putFloat32le v2
      mapM_ S.putFloat32le v3
      S.put (0x00 :: Word16)

newtype TriangleList a = TriangleList [a]

instance S.Serialize a => S.Serialize (TriangleList a) where
  get = undefined
  put (TriangleList list) = mapM_ S.put list

writeHeader :: Handle -> IO ()
writeHeader h = BS.hPut h (S.encode Header)

putHeader :: BS.Builder
putHeader = mconcat (replicate 80 $ BS.word8 0)

putLength :: Int -> BS.Builder
putLength = BS.word32LE . fromIntegral

putVector :: U.Vector (V3 Double) -> BS.Builder
putVector = mconcat . map putTriangle . package . U.toList

putTriangle :: Triangle -> BS.Builder
putTriangle (Triangle v1 v2 v3) =
  putVertex 0 <> putVertex v1 <> putVertex v2 <> putVertex v3 <> BS.word16LE 0

putVertex :: V3 Float -> BS.Builder
putVertex (V3 x y z) = BS.floatLE x <> BS.floatLE y <> BS.floatLE z

writeLength :: Handle -> Int -> IO ()
writeLength h i = BS.hPut h (S.encode $ Length i)

writeVector :: Handle -> U.Vector (V3 Double) -> IO ()
writeVector h = BS.hPut h . S.encode . TriangleList . package . U.toList

package :: [V3 Double] -> [Triangle]
package = tripleUp . map (fmap double2Float)
    where tripleUp (x:y:z:xs) = Triangle x y z : tripleUp xs
          tripleUp [] = []
          tripleUp _ = error "list doesn't contain a triple multiple"

overwriteLength :: FilePath -> Int -> IO ()
overwriteLength fp i =
  do
    withBinaryFile fp ReadWriteMode
      (\h -> hSeek h AbsoluteSeek 80 >> BS.hPutBuilder h (putLength i))
