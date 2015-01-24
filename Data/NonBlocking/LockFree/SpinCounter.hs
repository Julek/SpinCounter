
{-|
Module      : SpinCounter
Description : Implementation of a lock-free Spin Counter.
License     : BSD3
Maintainer  : Julian Sutherland (julian.sutherland10@imperial.ac.uk)

An implementation of a lock-free spin counter. Works with any monad that has atomically modificable references.
-}

module Data.NonBlocking.LockFree.SpinCounter(SpinCounter(), SpinCounterIO, SpinCounterSTM, newSpinCounter, incSpinCounter, readSpinCounter) where

import Control.Concurrent.STM (STM())
import Control.Concurrent.STM.TVar (TVar())
import Control.Monad.Loops(whileM_)
import Control.Monad.Ref(MonadAtomicRef, newRef, readRef, writeRef, atomicModifyRef)
import Data.IORef(IORef)

-- |SpinCounter inside the IO Monad.
type SpinCounterIO = SpinCounter IORef
-- |SpinCounter inside the STM Monad.
type SpinCounterSTM = SpinCounter TVar

-- |A lock-free concurrent Spin counter usable in any monad, m, that is paired with a reference type, r, by an instance of 'MonadAtomicRef'. Can use Specializations 'SpinCounterIO' and 'SpinCounterSTM'
data SpinCounter r = SpinCounter (r Integer)

-- |Creates a new instance of the 'SpinCounter' data type initialized to value of the input to the function, an instance of the class 'Integral'.
{-# SPECIALIZE newSpinCounter :: (Integral a) => a -> IO (SpinCounterIO)   #-}
{-# SPECIALIZE newSpinCounter :: (Integral a) => a -> STM (SpinCounterSTM) #-}
newSpinCounter :: (MonadAtomicRef r m, Integral a) => a -> m (SpinCounter r)
newSpinCounter n = newRef (toInteger n) >>= return . SpinCounter

-- |Increments an instance of the 'SpinCounter' data type by one in a lock-free manner.
{-# SPECIALIZE incSpinCounter :: SpinCounterIO -> IO ()   #-}
{-# SPECIALIZE incSpinCounter :: SpinCounterSTM -> STM () #-}
incSpinCounter :: (MonadAtomicRef r m) => SpinCounter r -> m ()
incSpinCounter (SpinCounter ref) = do
  b <- newRef False
  whileM_ (readRef b >>= return . not) $ do
    v <- readRef ref
    cas ref v (v+1) >>= writeRef b

-- |Reads the value of an instance of the 'SpinCounter' data type in a lock-free manner.
{-# SPECIALIZE readSpinCounter :: (Num a) => SpinCounterIO -> IO a   #-}
{-# SPECIALIZE readSpinCounter :: (Num a) => SpinCounterSTM -> STM a #-}
readSpinCounter :: (MonadAtomicRef r m, Num a) => SpinCounter r -> m a
readSpinCounter (SpinCounter ref) = readRef ref >>= return . fromInteger

{-# SPECIALIZE cas :: IORef Integer -> Integer -> Integer -> IO Bool   #-}
{-# SPECIALIZE cas :: TVar Integer -> Integer -> Integer -> STM Bool   #-}
cas :: (MonadAtomicRef r m, Eq a) => r a -> a -> a -> m Bool
cas ref comp rep = atomicModifyRef ref (\val -> let b = val == comp in (if b then rep else val, b))
