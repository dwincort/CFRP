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
--  A standard definition is get and put
class Resource rt a b | rt -> a, rt -> b where
  get :: rt -> IO b
  put :: rt -> a -> IO ()
  rsf :: (MonadWriter [RData] m, MonadIO m) => rt -> MSF m {-S rt-} a b
  rsf rt = pipe (\x -> tell [RData rt x] >> liftIO (get rt))

--class Resource rt a [b] => BResource rt a b | rt -> a, rt -> b where
--  bprocess :: rt -> a -> IO (Maybe b)
--  bprocess rt a = process rt a >>= (return . listToMaybe)
--  brsf :: MonadIO m => rt -> MSF m {-S rt-} a b
--  brsf rt = pipe (liftIO . f) where
--    {- Should that be a threadDelay instead of a yield? -}
--    f a = bprocess rt a >>= maybe (yield >> f a) return
--instance Resource rt a [b] => BResource rt a b

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
                    process (RDataW rt a) = put rt a
                    process (RData rt a)  = put rt a
  in forM_ (rdbrs ++ rdws) process


-----------------------------------------------------------
------------------------ Wormholes ------------------------
-----------------------------------------------------------
--  The WormholeData type is used to keep track of the contents of the wormhole
--  The WData type is a wrapper to allow multiple WormholeDatas to be kept together
type WormholeData t = IORef ([t],[t])
--data WData = forall t. WData (WormholeData t)

--  The Whitehole and Blackhole types hold WormholeData along with a phantom 
--  type that functions as the resource type for safety purposes.
newtype Whitehole rt t = Whitehole (WormholeData t)
newtype Blackhole rt t = Blackhole (WormholeData t)

instance Resource (Whitehole rt t) () [t] where
  get (Whitehole wd)   = readIORef wd >>= (return . snd)
  put (Whitehole wd) _ = atomicModifyIORef wd $ \(b,w) -> (([],b), ())
  rsf rt = pipe (\x -> tell [RDataW rt x] >> liftIO (get rt))

--instance BResource (Whitehole rt t) () t where
--  bprocess (Whitehole wd) () = atomicModifyIORef wd f where 
--    f (b,[])  = (([],b), Nothing)
--    f (b,h:l) = ((b,l),  Just h)

instance Resource (Blackhole rt t) t () where
  get (Blackhole wd)   = return ()
  put (Blackhole wd) t = atomicModifyIORef wd $ \(b,w) -> ((b++[t],w), ())



-- This function is the generic version of letW.
-- That is, it requires a WData handler.
letW :: forall m rw rb t a b. MonadIO m => 
       [t] -> (Whitehole rw t -> Blackhole rb t -> MSF m a b) -> MSF m a b
letW t inner = initialAction (liftIO $ newIORef ([],t)) $ \ref -> 
  inner (Whitehole ref) (Blackhole ref)
    
-- This function encapsulates the wormhole data updating that should be done 
-- during the "time step" of a signal function.  See Ft-Time transition.
--stepWHData :: [WData] -> IO ()
--stepWHData wds = forM_ wds (\(WData wd) -> atomicModifyIORef wd $ \(b,w) -> (([],w++b), ()))

-----------------------------------------------------------
------------------------- Forking -------------------------
-----------------------------------------------------------

-- This function is the generic version of forkSF.
-- That is, it requires a ThreadId handler as well as a function to run 
-- the forked signal functions.
-- Note that the use of myThreadId is a hack.  We need an initial value 
-- for the ThreadId input to h, and myThreadId shouldn't cause any problems.
forkSF' :: MonadIO m => (ThreadId -> m Bool) -> 
                        (MSF m () () -> m ThreadId) ->
                        MSF m () () -> MSF m a a
forkSF' haveIForked forkRun e = initialAction (liftIO myThreadId) $ \tid -> MSF (h e tid) where 
  h e tid x = do b <- haveIForked tid
                 newTid <- if b then return tid else forkRun e
                 return (x, MSF (h e newTid))


