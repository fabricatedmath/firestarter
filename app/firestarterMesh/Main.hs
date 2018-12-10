module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import qualified Data.ByteString.Builder as BS
import Data.Foldable
import Data.Monoid ((<>))
import qualified Data.Vector.Unboxed as U

import GHC.Float

import System.IO

import Linear

import System.Environment

import JuliaQ
import Firestarter
import Lib

main :: IO ()
main =
  do
    [gridSizeS,outputFile] <- getArgs
    let
      gridSize = read gridSizeS :: Double --0.005
      j = Julia (V3 2 2 2) gridSize
      (p1,p2) = lastTwoOnGrid j gridSize
    chan <- newTChanIO
    let fileName = outputFile ++ show gridSize ++ ".stl"
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
                      BS.hPutBuilder h (putVector v)
                      let i' = U.length v + i in i' `seq` go i'
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

type Triangle = V3 (V3 Float)

putHeader :: BS.Builder
putHeader = mconcat (replicate 80 $ BS.word8 0)

putLength :: Int -> BS.Builder
putLength = BS.word32LE . fromIntegral

putVector :: U.Vector (V3 Double) -> BS.Builder
putVector = mconcat . map putTriangle . package . U.toList

putTriangle :: Triangle -> BS.Builder
putTriangle tri =
  putVertex 0 <> fold (fmap putVertex tri) <> BS.word16LE 0

putVertex :: V3 Float -> BS.Builder
putVertex (V3 x y z) = BS.floatLE x <> BS.floatLE y <> BS.floatLE z

package :: [V3 Double] -> [Triangle]
package = tripleUp . map (fmap double2Float)
    where tripleUp (x:y:z:xs) = V3 x y z : tripleUp xs
          tripleUp [] = []
          tripleUp _ = error "list doesn't contain a triple multiple"
