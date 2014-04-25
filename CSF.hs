{-# LANGUAGE Arrows, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, TypeSynonymInstances, OverlappingInstances #-}

module CSF (
  CSF, runCSF, stepCSF, 
  forkSF, 
  async, buffer, parTo, spar, 
  --Reexporting
  Resource (..), --BResource (..), 
  letW,
  RData, Whitehole, Blackhole
) where

import Control.Arrow
import Control.Concurrent.MonadIO

import Data.IORef
import Data.Maybe (isJust, listToMaybe, catMaybes)

import Control.Monad.Writer.Strict

import SF
import MSF
import CFRP

-----------------------------------------------------------
----------------------- ThreadState -----------------------
-----------------------------------------------------------

-- The ThreadState object is essentially a tree of thread information 
-- that is extended in choice operations and forking.  Choice will 
-- also have the capability of accessing the thread tree in order to 
-- kill all child threads.

newtype ThreadState = ThreadState (IORef ([ThreadId], [ThreadState]))
  deriving Eq

-- Construct a new ThreadState object
newThreadState :: IO ThreadState
newThreadState = do
  ref <- newIORef ([],[])
  return $ ThreadState ref

-- Given the incoming thread state and a stored thread ID, this function 
-- returns True iff the thread ID is in this thread state's thread ID list.
haveIForked :: ThreadState -> ThreadId -> IO Bool
haveIForked (ThreadState ts) t = do
  (tids, _) <- readIORef ts
  return $ elem t tids

-- Given the incoming thread state and a stored thread state, this function 
-- returns True iff the thread state is in this thread state's thread state list.
haveIForked' :: ThreadState -> ThreadState -> IO Bool
haveIForked' (ThreadState ts) ts' = do
  (_, tss) <- readIORef ts
  return $ elem ts' tss

-- getThreads recursively looks through all ThreadState references of the 
-- given thread state to return all child thread IDs.
getThreads :: ThreadState -> IO [ThreadId]
getThreads (ThreadState ts) = do
  (tids, tss) <- readIORef ts
  tids' <- mapM getThreads tss
  return $ tids ++ concat tids'

-- Adds a thread state (second argument) to a thread state (first argument).
-- The output is the same as the second argument, which is convenient for 
-- certain callers.
addTS :: ThreadState -> ThreadState -> IO ThreadState
addTS (ThreadState ts) ts' = atomicModifyIORef ts $ \(tids, tss) -> ((tids, ts':tss),ts')

-- Adds a thread ID and accompanying thread state (second argument) 
-- to a thread state (first argument).
addTid :: ThreadState -> (ThreadId, ThreadState) -> IO ()
addTid (ThreadState ts) (tid, ts') = atomicModifyIORef ts $ \(tids, tss) -> ((tid:tids, ts':tss),())

-----------------------------------------------------------
------------------------- CMonad --------------------------
-----------------------------------------------------------

type CSF = MSF CMonad
-- CMonad is an extension to IO to allow tracking of wormhole data 
-- and thread data.  It is essentially just a reader and writer monad, 
-- but we define it explicitly.
newtype CMonad a = CMonad {unC :: ThreadState -> IO ([RData], a)}

instance Monad CMonad where
  return a = CMonad $ const $ return ([],a)
  (CMonad c) >>= f = CMonad $ \ts -> do 
    (rds,  a) <- c ts
    (rds', b) <- (unC $ f a) ts
    return (rds++rds', b)

instance MonadWriter [RData] CMonad where
  tell w = CMonad $ const $ return (w,())
  listen (CMonad c) = CMonad $ \ts -> do
    (rds, a) <- c ts
    return (rds, (a, rds))
  pass (CMonad c) = CMonad $ \ts -> do
    (rds, (a, ww)) <- c ts
    return (ww rds, a)

instance MonadIO CMonad where
  liftIO a = CMonad $ const $ a >>= (\v -> return ([],v))

-- The ArrowChoice instance is overridden to allow arrow choice to kill 
-- child threads of unchosen branches.
instance ArrowChoice CSF where
  left msf = initialAction (liftIO newThreadState) (\ts -> MSF (h msf ts)) where
    h msf tsi x = 
      case x of
        Left x' -> CMonad $ \ts -> do 
          b <- haveIForked' ts tsi
          ts' <- if b then return tsi else newThreadState >>= addTS ts 
          (rdata, (y, msf')) <- unC (msfFun msf x') ts'
          return (rdata, (Left y, MSF (h msf' ts')))
        Right y -> do 
          tids <- liftIO $ getThreads tsi
          liftIO $ mapM_ killThread tids
          return (Right y, MSF (h msf tsi))
  f ||| g = f +++ g >>> arr untag
        where
          untag (Left x) = x
          untag (Right y) = y


-- Running and stepping through CSFs
runCSF :: a -> ThreadState -> CSF a b -> IO b
runCSF a ts f = run f where 
  run (MSF f) = do 
    (rds, (y, f')) <- (unC $ f a) ts
    stepRData rds
    run f'

runCSF' = runCSF ()

stepCSF :: CSF a b -> [a] -> IO [b]
stepCSF csf inp = newThreadState >>= stepCSF' csf inp where
  stepCSF' _ [] ts = getThreads ts >>= mapM_ killThread >> return []
  stepCSF' (MSF f) (x:xs) ts = do 
    (rds, (y, f')) <- (unC $ f x) ts
    -- This next line performs the Ft-Time transition
    stepRData rds
    ys <- stepCSF' f' xs ts
    -- This next line kills all spawned threads
    return (y:ys)


-- CMonad specific version of forkSF (for ease of coding)
forkSF :: CSF () () -> CSF a a
forkSF = forkSF' (\tid -> getThreadState >>= (\ts -> liftIO $ haveIForked ts tid)) forkRun
  where
    getThreadState :: CMonad ThreadState
    getThreadState = CMonad $ \ts -> return ([],ts)
    forkRun :: CSF () () -> CMonad ThreadId
    forkRun csf = CMonad $ \ts -> do
      newTS <- newThreadState
      tid <- forkIO $ runCSF' newTS csf
      addTid ts (tid, newTS)
      return ([], tid)

-----------------------------------------------------------
---------------------- Constructions ----------------------
-----------------------------------------------------------

async :: CSF [a] b -> CSF a [b]
async sf = letW [] $ \wi bi -> letW [] $ \wo bo -> forkSF (g wi bo) >>> (f bi wo)
  where f bi wo = rsf bi >>> rsf wo
        g wi bo = rsf wi >>> sf >>> rsf bo

--par :: CSF a b -> CSF a c -> CSF a (b,c)
--sf1 `par` sf2 = letW [] $ \wi bi -> letW [] $ \wo bo -> forkSF (f bi wo) (g wi bo)
--  where f bi wo = proc a -> do
--            () <- rsf bi -< a
--            b <- sf1 -< a
--            c <- brsf wo -< ()
--            returnA -< (b,c)
--        g wi bo = brsf wi >>> sf2 >>> rsf bo

buffer :: CSF a [Maybe b] -> CSF a (Maybe b)
buffer sf = letW [[]] $ \rw rb -> proc x -> do
    (b:_) <- rsf rw -< ()
    elementsNew <- sf -< x
    let (r, bNew) = case (b ++ filter isJust elementsNew) of
                      [] -> (Nothing, [])
                      (y:ys) -> (y,ys)
    _ <- rsf rb -< bNew
    returnA -< r

parTo :: CSF a (Maybe b) -> CSF (Maybe b) () -> CSF a ()
sf1 `parTo` sf2 = letW [] $ \w b -> forkSF (g w) >>> f b
  where f b = sf1 >>> rsf b
        g w = buffer (rsf w) >>> sf2

spar :: CSF (Maybe a) (Maybe b) 
     -> CSF (Maybe a) (Maybe c) 
     -> CSF (Maybe a) (Maybe (Either b c))
sf1 `spar` sf2 = letW [] $ \rw rb -> proc a -> do
    done <- rsf rw -< ()
    if null done then do
      el <- buffer (async (arr collapse >>> sf1)) -< a
      er <- buffer (async (arr collapse >>> sf2)) -< a
      case (el, er) of
        (Just b, _) -> do _ <- rsf rb -< ()
                          returnA -< Just (Left b)
        (_, Just c) -> do _ <- rsf rb -< ()
                          returnA -< Just (Right c)
        _ -> returnA -< Nothing
    else do
      _ <- rsf rb -< ()
      returnA -< Nothing
  where collapse = listToMaybe . catMaybes


