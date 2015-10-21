
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, FlexibleInstances, MultiParamTypeClasses #-}
module MSF where
import TypeSet
import Arrow
import Control.Concurrent.MonadIO


-- A MSF is a signal function that that can perform actions over an embedded monad
data MSF m (r :: [*]) a b = MSF { msfFun :: (a -> m (b, MSF m r a b)) }


-----------------------------------------------------------
--------------------- Arrow instances ---------------------
-----------------------------------------------------------
instance Monad m => Category (MSF m) where
  iden = MSF h where h x = return (x, MSF h)
  MSF g <<< MSF f = MSF (h f g)
    where h f g x    = do (y, MSF f') <- f x
                          (z, MSF g') <- g y
                          return (z, MSF (h f' g'))

instance Monad m => Arrow (MSF m) where
  arr f = MSF h 
    where h x = return (f x, MSF h)
  first (MSF f) = MSF (h f)
    where h f (x, z) = do (y, MSF f') <- f x
                          return ((y, z), MSF (h f'))
  f &&& g = MSF (h f g)
    where
      h f g x = do
        (y, f') <- msfFun f x
        (z, g') <- msfFun g x 
        return ((y, z), MSF (h f' g'))
  f *** g = MSF (h f g)
    where
      h f g x = do
        (y, f') <- msfFun f (fst x)
        (z, g') <- msfFun g (snd x) 
        return ((y, z), MSF (h f' g'))

instance Monad m => ArrowChoice (MSF m) where
  left msf = MSF (h msf)
    where h msf x = case x of
                      Left x' -> do (y, msf') <- msfFun msf x'
                                    return (Left y, MSF (h msf'))
                      Right y -> return (Right y, MSF (h msf))
  f ||| g = MSF (h f g)
    where h f g x = case x of
                      Left  b -> do (d, f') <- msfFun f b
                                    return (d, MSF (h f' g))
                      Right c -> do (d, g') <- msfFun g c
                                    return (d, MSF (h f g'))

instance Monad m => ArrowAction m (MSF m) where
    initialAction mx f = MSF $ \a -> do
      x <- mx
      msfFun (f x) a

-----------------------------------------------------------
-------------------- MSF Constructors ---------------------
-----------------------------------------------------------

--source :: Monad m => m c ->         MSF m () c
--sink   :: Monad m => (b -> m ()) -> MSF m b  ()
pipe   :: Monad m => (b -> m c) ->  MSF m r b  c
--source f = MSF h where h _ = f   >>= return . (\x -> (x, MSF h))
--sink   f = MSF h where h x = f x >> return ((), MSF h)
pipe   f = MSF h where h x = f x >>= return . (\x -> (x, MSF h))
--
--sourceE :: Monad m => m c ->         MSF m (Maybe ()) (Maybe c)
--sinkE   :: Monad m => (b -> m ()) -> MSF m (Maybe b)  (Maybe ())
--pipeE   :: Monad m => (b -> m c) ->  MSF m (Maybe b)  (Maybe c)
--sourceE f = MSF h where h = maybe (return (Nothing, MSF h)) (\_ -> f   >>= return . (\c -> (Just c, MSF h)))
--sinkE   f = MSF h where h = maybe (return (Nothing, MSF h)) (\b -> f b >>  return (Just (), MSF h))
--pipeE   f = MSF h where h = maybe (return (Nothing, MSF h)) (\b -> f b >>= return . (\c -> (Just c, MSF h)))
--
--initialAction :: Monad m => m x -> (x -> MSF m a b) -> MSF m a b
--initialAction mx f = MSF $ \a -> do
--  x <- mx
--  msfFun (f x) a


-----------------------------------------------------------
----------------------- MSF Runners -----------------------
-----------------------------------------------------------

stepMSF :: Monad m => MSF m r a b -> [a] -> m [b]
stepMSF _ [] = return []
stepMSF (MSF f) (x:xs) = do 
  (y, f') <- f x
  ys <- stepMSF f' xs
  return (y:ys)

stepMSF' :: Monad m => MSF m r a b -> [a] -> m ([b], MSF m r a b)
stepMSF' g [] = return ([], g)
stepMSF' (MSF f) (x:xs) = do 
  (y, f') <- f x
  (ys, g) <- stepMSF' f' xs
  return (y:ys, g)

data Stream m b = Stream { stream :: m (b, Stream m b) }
streamMSF :: Monad m => MSF m r a b -> [a] -> Stream m b
streamMSF (MSF f) (x:xs) = Stream $ do 
  (y, f') <- f x
  return (y, streamMSF f' xs)

runMSF :: Monad m => a -> MSF m r a b -> m b
runMSF a f = run f where run (MSF f) = do f a >>= run . snd

runMSF' :: Monad m => MSF m r () b -> m b
runMSF' = runMSF ()
