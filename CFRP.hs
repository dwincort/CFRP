{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module CFRP (
  Resource (..), --BResource (..), 
  RData, Whitehole, Blackhole, 
  letW, stepRData, 
  forkSF'
) where

import Control.Arrow
import Control.Concurrent.MonadIO

import Data.IORef
import Control.Monad (forM_)
import Data.Maybe (listToMaybe)
import Data.List (partition)

import Control.Monad.Writer.Strict

import SF
import MSF


-----------------------------------------------------------
-------------------- Resource classes ---------------------
-----------------------------------------------------------
--  A standard definition is get and put.
--  get behaves like a peek, providing data without the resource "knowing".
--  put provides the data that was peeked along with new input data for the 
--   resource to "update" the resource.
class Resource rt a b | rt -> a, rt -> b where
  get :: rt -> IO b
  put :: rt -> b -> a -> IO ()
  rsf :: (MonadWriter [RData] m, MonadIO m) => rt -> MSF m {-S rt-} a b
  rsf rt = pipe $ \x -> do
    y <- liftIO $ get rt
    tell [RData rt x y]
    return y

-- The RData type feels like it should only need one constructor, 
-- but whitehole data _must_ be processed _after_ blackhole data, 
-- so we make a separate RDataW constructor for whitehole data and 
-- then use it to sort a list of RData (as seen in stepRData below).
data RData = forall rt a b . Resource rt a b => RData  rt a b
           | forall rt a b . Resource rt a b => RDataW rt a b

-- This function encapsulates the resource data updating that should be done 
-- during the "time step" of a signal function.  See Ft-Time transition.
stepRData :: [RData] -> IO ()
stepRData rds = let (rdws, rdbrs) = partition isRDataW rds
                    isRDataW (RDataW _ _ _) = True
                    isRDataW (RData  _ _ _) = False
                    process (RDataW rt x y) = put rt y x
                    process (RData  rt x y) = put rt y x
  in forM_ (rdbrs ++ rdws) process


-----------------------------------------------------------
------------------------ Wormholes ------------------------
-----------------------------------------------------------
--  The WormholeData type is used to keep track of the contents of the wormhole
--  The WData type is a wrapper to allow multiple WormholeDatas to be kept together
-- TODO: Change this to a Seq, or something else with constant time access to both ends
type WormholeData t = IORef [t]

--  The Whitehole and Blackhole types hold WormholeData along with a phantom 
--  type that functions as the resource type for safety purposes.
newtype Whitehole rt t = Whitehole (WormholeData t)
newtype Blackhole rt t = Blackhole (WormholeData t)

instance Resource (Whitehole rt t) () [t] where
  get (Whitehole wd)   = readIORef wd
  put (Whitehole wd) lst _ = atomicModifyIORef wd $ \w -> (drop (length lst) w, ())
  rsf rt = pipe $ \x -> do
    y <- liftIO $ get rt
    tell [RDataW rt x y]
    return y

instance Resource (Blackhole rt t) t () where
  get (Blackhole wd)   = return ()
  put (Blackhole wd) _ t = atomicModifyIORef wd $ \w -> (w++[t], ())


letW :: forall m rw rb t a b. MonadIO m => 
       [t] -> (Whitehole rw t -> Blackhole rb t -> MSF m a b) -> MSF m a b
letW t inner = initialAction (liftIO $ newIORef t) $ \ref -> 
  inner (Whitehole ref) (Blackhole ref)
    
-----------------------------------------------------------
------------------------- Forking -------------------------
-----------------------------------------------------------

-- This function is the generic version of forkSF.
-- That is, it requires a function to run the forked signal function.
forkSF' :: MonadIO m => (MSF m () () -> m ()) ->
                        MSF m () () -> MSF m a a
forkSF' forkRun e = initialAction (forkRun e) $ const $ arr id


