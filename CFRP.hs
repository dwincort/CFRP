{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module CFRP (
  Resource (..), BResource (..), 
  WData, Whitehole, Blackhole, 
  letW', stepWHData, 
  forkSF'
) where

import Control.Arrow
import Control.Concurrent.MonadIO

import Data.IORef
import Control.Monad (forM_)

import SF
import MSF


-----------------------------------------------------------
-------------------- Resource classes ---------------------
-----------------------------------------------------------
--  A standard definition is process (and optionally bprocess if the resource can block)
class Resource rt a b | rt -> a, rt -> b where
  process :: rt -> a -> IO b
  rsf :: MonadIO m => rt -> MSF m {-S rt-} a b
  rsf = pipe . (liftIO .) . process

class Resource rt a [b] => BResource rt a b | rt -> a, rt -> b where
  bprocess :: rt -> a -> IO (Maybe b)
  brsf :: MonadIO m => rt -> MSF m {-S rt-} a b
  brsf rt = pipe (liftIO . f) where
    {- Should that be a threadDelay instead of a yield? -}
    f a = bprocess rt a >>= maybe (yield >> f a) return


-----------------------------------------------------------
------------------------ Wormholes ------------------------
-----------------------------------------------------------
--  The WormholeData type is used to keep track of the contents of the wormhole
--  The WData type is a wrapper to allow multiple WormholeDatas to be kept together
type WormholeData t = IORef ([t],[t])
data WData = forall t. WData (WormholeData t)

--  The Whitehole and Blackhole types hold WormholeData along with a phantom 
--  type that functions as the resource type for safety purposes.
newtype Whitehole rt t = Whitehole (WormholeData t)
newtype Blackhole rt t = Blackhole (WormholeData t)

instance Resource (Whitehole rt t) () [t] where
  process (Whitehole wd) () = atomicModifyIORef wd $ \(b,w) -> ((b,[]), w)

instance BResource (Whitehole rt t) () t where
  bprocess (Whitehole wd) () = atomicModifyIORef wd f where 
    f (b,[])  = (([],b), Nothing)
    f (b,h:l) = ((b,l),  Just h)

instance Resource (Blackhole rt t) t () where
  process (Blackhole wd) t = atomicModifyIORef wd $ \(b,w) -> ((b++[t],w), ())

-- This function is the generic version of letW.
-- That is, it requires a WData handler.
letW' :: forall m rw rb t a b. MonadIO m => (WData -> m ()) -> 
        [t] -> (Whitehole rw t -> Blackhole rb t -> MSF m a b) -> MSF m a b
letW' handler t inner = initialAction (liftIO $ newIORef ([],t)) $ \ref -> 
  MSF (h $ WData ref) >>> inner (Whitehole ref) (Blackhole ref) where
    h wd x = handler wd >> return (x, MSF (h wd))
    
-- This function encapsulates the wormhole data updating that should be done 
-- during the "time step" of a signal function.  See Ft-Time transition.
stepWHData :: [WData] -> IO ()
stepWHData wds = forM_ wds (\(WData wd) -> atomicModifyIORef wd $ \(b,w) -> (([],w++b), ()))

-----------------------------------------------------------
------------------------- Forking -------------------------
-----------------------------------------------------------

-- This function is the generic version of forkSF.
-- That is, it requires a ThreadId handler as well as a function to run 
-- the forked signal functions.
forkSF' :: MonadIO m => (ThreadId -> m ()) -> (MSF m () () -> IO ()) ->
                        MSF m a b -> MSF m () () -> MSF m a b
forkSF' handler runner e ef = initialAction (liftIO (forkIO $ runner ef) >>= handler) $ const e

