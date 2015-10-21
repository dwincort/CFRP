{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module CFRP (
  Resource (..), --BResource (..), 
  RData, Whitehole, Blackhole, 
  letW, stepRData, 
  forkSF'
) where

import TypeSet
import Arrow
import Control.Concurrent.MonadIO

import Data.IORef
import Control.Monad (forM_)
import Data.Maybe (listToMaybe)
import Data.List (partition)

import Control.Monad.Writer.Strict

import SF
import MSF

import Unsafe.Coerce


-----------------------------------------------------------
-------------------- Resource classes ---------------------
-----------------------------------------------------------
--  A standard definition is get and put.
--  get behaves like a peek, providing data without the resource "knowing".
--  put provides the data that was peeked along with new input data for the 
--   resource to "update" the resource.
class Resource rt a b | rt -> a, rt -> b where
  get :: rt -> IO b
  put :: rt -> a -> IO ()
  rsf :: (MonadWriter [RData] m, MonadIO m) => rt -> MSF m '[rt] a b
  rsf rt = pipe $ \x -> do
    tell [RData rt x]
    liftIO $ get rt

-- The RData type feels like it should only need one constructor, 
-- but whitehole data _must_ be processed _after_ blackhole data, 
-- so we make a separate RDataW constructor for whitehole data and 
-- then use it to sort a list of RData (as seen in stepRData below).
data RData = forall rt a b . Resource rt a b => RData  rt a
           | forall rt a b . Resource rt a b => RDataW rt a

-- This function encapsulates the resource data updating that should be done 
-- during the "time step" of a signal function.  See Ft-Time transition.
stepRData :: [RData] -> IO ()
stepRData rds = let (rdws, rdbrs) = partition isRDataW rds
                    isRDataW (RDataW _ _) = True
                    isRDataW (RData  _ _) = False
                    process (RDataW rt x) = put rt x
                    process (RData  rt x) = put rt x
  in forM_ (rdbrs ++ rdws) process


-----------------------------------------------------------
------------------------ Wormholes ------------------------
-----------------------------------------------------------
--  The WormholeData type is used to keep track of the contents of the wormhole
--  The WData type is a wrapper to allow multiple WormholeDatas to be kept together
-- TODO: Change this to a Seq, or something else with constant time access to both ends
type WormholeData t = (IORef [t], IORef [t])

--  The Whitehole and Blackhole types hold WormholeData along with a phantom 
--  type that functions as the resource type for safety purposes.
newtype Whitehole rt t = Whitehole (WormholeData t)
newtype Blackhole rt t = Blackhole (WormholeData t)

instance Resource (Whitehole r t) () [t] where
  get (Whitehole (_,w))   = readIORef w
  put (Whitehole (b,w)) _ = do
    bdata <- atomicModifyIORef b $ \l -> ([], l)
    atomicModifyIORef w $ \_ -> (reverse bdata, ())
  rsf rt = pipe $ \x -> do
    tell [RDataW rt x]
    liftIO $ get rt

instance Resource (Blackhole rt t) t () where
  get (Blackhole _)   = return ()
  put (Blackhole (b,_)) t = atomicModifyIORef b $ \l -> (t:l, ())


letW :: forall m rt r r' r'' t a b. 
       (MonadIO m, LRemove (Whitehole rt t) r r', LRemove (Blackhole rt t) r' r'') => 
       [t] -> (Whitehole rt t -> Blackhole rt t -> MSF m r a b) -> MSF m r'' a b
letW t inner = lremove (undefined::Blackhole rt t) $ 
  lremove (undefined::Whitehole rt t) $ 
  initialAction makerefs $ \refs -> 
    inner (Whitehole refs) (Blackhole refs)  where
      makerefs :: m (IORef [t1], IORef [t])
      makerefs = do
          b <- liftIO $ newIORef []
          w <- liftIO $ newIORef t
          return (b,w)
    
-----------------------------------------------------------
------------------------- Forking -------------------------
-----------------------------------------------------------

-- This function is the generic version of forkSF.
-- That is, it requires a function to run the forked signal function.
forkSF' :: forall m r a. MonadIO m => 
    (MSF m r () () -> m ()) ->
     MSF m r () () -> MSF m r a a
forkSF' forkRun e = initialAction (forkRun e) $ const $ 
    ((unsafeCoerce (iden :: MSF m '[] a a)) :: MSF m r a a)


