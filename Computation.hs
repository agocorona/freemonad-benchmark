{-# LANGUAGE FlexibleContexts, RankNTypes, FlexibleInstances , DataKinds  #-}
module Computation where

import Base
import Control.Monad
import qualified Control.Monad.State.Strict as MTL
import Control.Monad.Free.VanLaarhovenE
import qualified Streaming.Prelude as S
import qualified Streaming as S
import Streaming (Stream(..), Of(..))
import Control.Monad.Trans
import Transient.Base
import Control.Applicative

tcomputation :: Int -> TransIO()
tcomputation n=  forM_ [1..n] $ \_ -> do
    s <- getSData <|> return (1 ::Int)
    setSData $! s + 1

computation
  :: (Monad m, MonadFree F m)
  => Int
  -> m ()
computation n = forM_ [1..n] $ \_ -> do
  s <- get
  put $! s + 1
{-#INLINABLE computation #-}

mtlComputationIO ::  Int -> MTL.StateT Int IO ()
mtlComputationIO n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1

mtlComputationId:: Int -> MTL.State Int ()
mtlComputationId n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1

mtlComputationGeneral:: Monad m => Int -> MTL.StateT Int m ()
mtlComputationGeneral n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1

mtlComputationGeneralInlinable:: Monad m => Int -> MTL.StateT Int m ()
mtlComputationGeneralInlinable n = forM_ [1..n] $ \_ -> do
    s <- MTL.get
    MTL.put $! s + 1
{-#INLINABLE mtlComputationGeneralInlinable #-}

streamingComp :: Monad m => Int -> Stream (Of Int) (MTL.StateT Int m) ()
streamingComp = lift . mtlComputationGeneral

streamingCompIO :: Int -> Stream (Of Int) (MTL.StateT Int IO) ()
streamingCompIO = lift . mtlComputationIO

streamingCompId :: Int -> Stream (Of Int) (MTL.State Int) ()
streamingCompId = lift . mtlComputationId

computation2
  :: (Monad m, MonadFree F m)
  => Int
  -> m ()
computation2 n =
  if n == 0
    then return ()
    else do
      computation2 (n-1)
      s <- get
      put $! s + 1

mtlComputation2 :: Monad m => Int -> MTL.StateT Int m ()
mtlComputation2 n =
  if n == 0
    then return ()
    else do
      mtlComputation2 (n-1)
      s <- MTL.get
      MTL.put $! s + 1


data State s m = State{getState :: m s, putState :: s -> m ()}

get_ :: HasEffect effects (State s) => Free effects s
get_  = liftF getState

put_ :: HasEffect effects (State s) => s -> Free effects ()
put_ s = liftF (\st -> putState st s)

vlComputation
    :: (HasEffect effects (State Int)) =>
       Int -> Free effects ()

vlComputation n = forM_ [1..n] $ \_ -> do
    s <- get_
    put_ $! s + (1::Int)


myState :: State s (MTL.State s)
myState = State {getState = MTL.get, putState = MTL.put}

stateInterp = myState .:. EmptyE

vl  :: Free '[State Int] a -> MTL.State Int a
vl = iterM stateInterp


