{-# LANGUAGE FlexibleContexts, RankNTypes#-}
module Main (main) where

import Base
import Computation
import qualified Free
import qualified Church
import qualified Codensity
import qualified NoRemorse
import qualified Freer
import Control.Monad
import qualified Control.Monad.State.Strict as MTL
import Data.Functor.Identity
import Criterion (bench, nf, nfIO, bgroup, Benchmark)
import Criterion.Main (defaultMain)
import qualified Control.Monad.Free.VanLaarhovenE as VL

n = 50

benchmarks
  :: (forall m . (Monad m, MonadFree F m) => Int -> m ())
  -> (Int -> MTL.State Int ())
  -> Int
  -> [Benchmark]
benchmarks computation mtlComputation n =
  [ 
  bench "Free" $ nf (flip Free.run 0 . computation) n
  , bench "Free/lazy" $ nf (flip Free.runLazily 0 . computation) n
  , bench "Chruch" $ nf (flip Church.run 0 . computation) n
  , bench "Codensity" $ nf (flip Codensity.run 0 . computation) n
  , bench "NoRemorse" $ nf (flip NoRemorse.run 0 . computation) n
  , bench "Freer" $ nf (flip Freer.run 0 . computation) n
  , bench "MTLGeneral" $ nf (flip MTL.runState 0 . mtlComputationGeneral) n
  , bench "MTLIdentity" $ nf (flip MTL.runState 0 . mtlComputationId) n
  , bench "MTLIO" $ nfIO (MTL.runStateT (mtlComputationIO n) 0) 
  , bench "VL" $ nf (flip MTL.runState 0 . vl . vlComputation) n

  ]

main :: IO ()
main = defaultMain
  [ bgroup "Right-assoc" $ benchmarks computation mtlComputationId n
  , bgroup "Left-assoc" $ benchmarks computation2 mtlComputation2 n
  ]
