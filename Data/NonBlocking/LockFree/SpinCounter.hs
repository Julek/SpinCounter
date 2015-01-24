module Data.NonBlocking.LockFree.SpinCounter(SpinCounter(), SpinCounterIO, SpinCounterSTM, newSpinCounter, incSpinCounter, readSpinCounter) where

import Control.Concurrent.STM (STM())
import Control.Concurrent.STM.TVar (TVar())
import Control.Monad.Loops(whileM_)
import Control.Monad.Ref(MonadAtomicRef, newRef, readRef, writeRef, atomicModifyRef)
import Data.IORef(IORef)

type SpinCounterIO = SpinCounter IORef
type SpinCounterSTM = SpinCounter TVar

data SpinCounter r = SpinCounter (r Integer)

{-# SPECIALIZE newSpinCounter :: IO (SpinCounterIO)   #-}
{-# SPECIALIZE newSpinCounter :: STM (SpinCounterSTM) #-}
newSpinCounter :: (MonadAtomicRef r m) => m (SpinCounter r)
newSpinCounter = newRef 0 >>= return . SpinCounter

{-# SPECIALIZE incSpinCounter :: SpinCounterIO -> IO ()   #-}
{-# SPECIALIZE incSpinCounter :: SpinCounterSTM -> STM () #-}
incSpinCounter :: (MonadAtomicRef r m) => SpinCounter r -> m ()
incSpinCounter (SpinCounter ref) = do
  b <- newRef False
  whileM_ (readRef b >>= return . not) $ do
    v <- readRef ref
    cas ref v (v+1) >>= writeRef b

{-# SPECIALIZE readSpinCounter :: (Num a) => SpinCounterIO -> IO a   #-}
{-# SPECIALIZE readSpinCounter :: (Num a) => SpinCounterSTM -> STM a #-}
readSpinCounter :: (MonadAtomicRef r m, Num a) => SpinCounter r -> m a
readSpinCounter (SpinCounter ref) = readRef ref >>= return . fromInteger

{-# SPECIALIZE cas :: IORef Integer -> Integer -> Integer -> IO Bool   #-}
{-# SPECIALIZE cas :: TVar Integer -> Integer -> Integer -> STM Bool   #-}
cas :: (MonadAtomicRef r m, Eq a) => r a -> a -> a -> m Bool
cas ref comp rep = atomicModifyRef ref (\val -> let b = val == comp in (if b then rep else val, b))
