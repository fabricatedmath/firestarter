module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad

import qualified Data.ByteString.Builder as BS
import Data.Foldable
import Data.Monoid ((<>))
import qualified Data.Vector.Unboxed as U

import GHC.Float

import System.Console.GetOpt
import System.IO

import Linear

import System.Environment
import System.Exit

import JuliaQ
import Firestarter
import Lib

startOptions :: Options
startOptions =
  Options
  { optGridSize = 0.005
  , optFilePrefix = ""
  }

data Options = Options
  { optGridSize :: Double
  , optFilePrefix :: String
  } deriving Show

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "g" ["gridsize"]
    (ReqArg
     (\arg opt -> return opt {optGridSize = read arg})
     "Double"
    ) "Grid size to use (default: 0.005)"
  , Option "p" ["prefix"]
    (ReqArg
     (\arg opt -> return opt {optFilePrefix = arg ++ "-"})
     "String"
    ) "Prefix for filename"
  , Option "h" ["help"]
    (NoArg
     (\_ -> do
         prg <- getProgName
         hPutStrLn stderr $ usageInfo prg options
         exitWith ExitSuccess
     )
    ) "Show help"
  ]

main :: IO ()
main =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let
      gridSize = optGridSize opts
      j = Julia (V3 2 2 2) gridSize
      (p1,p2) = lastTwoOnGrid j gridSize
      fileName = optFilePrefix opts ++ show gridSize ++ ".stl"
    chan <- newTChanIO
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
    let f v = (v, calcNormal v)
    stepFire j f gridSize [p1,p2] (atomically . writeTChan chan . Just)
    atomically $ writeTChan chan Nothing
    _done <- readMVar doneVar
    print "done"

type Triangle a = V3 (V3 a)

putHeader :: BS.Builder
putHeader = mconcat (replicate 80 $ BS.word8 0)

putLength :: Int -> BS.Builder
putLength = BS.word32LE . fromIntegral

putVector :: U.Vector (Triangle Double, V3 Double) -> BS.Builder
putVector =
  mconcat . map putTriangle . U.toList .
  U.map (\(t,n) -> (fmap fmap fmap double2Float t, fmap double2Float n))

calcNormal
  :: (Epsilon a, Floating a, Fractional a, RealFloat a)
  => Triangle a -> V3 a
calcNormal = juliaAnalyticNormal 100 200 initC . centroid
  where centroid :: Fractional a => Triangle a -> V3 a
        centroid t =
          let
            x = sum (fmap (^. _x) t) / 3
            y = sum (fmap (^. _y) t) / 3
            z = sum (fmap (^. _z) t) / 3
          in V3 x y z

putTriangle :: (Triangle Float, V3 Float) -> BS.Builder
putTriangle (tri,n) =
  putVertex n <> fold (fmap putVertex tri) <> BS.word16LE 0

putVertex :: V3 Float -> BS.Builder
putVertex (V3 x y z) = BS.floatLE x <> BS.floatLE y <> BS.floatLE z
