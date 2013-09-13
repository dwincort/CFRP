{-# LANGUAGE Arrows #-}

module CSF (
  CSF, runCSF, stepCSF, 
  forkSF, letW,
  async, async', par, parTo, spar, 
  --Reexporting
  Resource (..), BResource (..), 
  WData, Whitehole, Blackhole
) where

import Control.Arrow
import Control.Concurrent.MonadIO

import Data.IORef

import SF
import MSF
import CFRP

-----------------------------------------------------------
------------------------- CMonad --------------------------
-----------------------------------------------------------

type CSF a b = MSF CMonad a b
-- CMonad is an extension to IO to allow tracking of wormhole data 
-- and thread data.
newtype CMonad a = CMonad {unC :: IORef [ThreadId] -> IO ([WData], a)}

instance Monad CMonad where
  return a = CMonad $ const $ return ([],a)
  (CMonad c) >>= f = CMonad $ \tidRef -> do 
    (wds,  a) <- c tidRef
    (wds', b) <- (unC $ f a) tidRef
    return (wds++wds', b)

instance MonadIO CMonad where
  liftIO a = CMonad $ const $ a >>= (\v -> return ([],v))

-- The wormhole data and thread ID handlers for CMonad
wdHandler :: WData -> CMonad ()
wdHandler wd = CMonad $ const $ return ([wd],())

threadHandler :: ThreadId -> CMonad ()
threadHandler tid = CMonad $ \tidRef -> 
  atomicModifyIORef tidRef (\tids -> (tid:tids,())) >> return ([],())

getTIDRef :: CMonad (IORef [ThreadId])
getTIDRef = CMonad $ \tidRef -> return ([],tidRef)

-- Running and stepping through CSFs
runCSF :: a -> IORef [ThreadId] -> CSF a b -> IO b
runCSF a tidref f = run f where 
  run (MSF f) = do 
    (wds, (y, f')) <- (unC $ f a) tidref
    stepWHData wds
    run f'

runCSF' = runCSF ()

stepCSF :: CSF a b -> [a] -> IO [b]
stepCSF csf inp = newIORef [] >>= stepCSF' csf inp where
  stepCSF' _ [] tidRef = readIORef tidRef >>= mapM_ killThread >> return []
  stepCSF' (MSF f) (x:xs) tidRef = do 
    (wds, (y, f')) <- (unC $ f x) tidRef
    -- This next line performs the Ft-Time transition
    stepWHData wds
    ys <- stepCSF' f' xs tidRef
    -- This next line kills all spawned threads
    return (y:ys)


-- CMonad specific versions of letW and forkSF (for ease of coding)
letW = letW' wdHandler
forkSF e ef = initialAction getTIDRef (\tidRef -> forkSF' threadHandler (runCSF' tidRef) e ef)
--forkSF = (initialAction getTIDRef .) . flip . flip ((forkSF' threadHandler) . runCSF')

-----------------------------------------------------------
---------------------- Constructions ----------------------
-----------------------------------------------------------

async :: CSF [a] b -> CSF a [b]
async sf = letW [] $ \wi bi -> letW [] $ \wo bo -> forkSF (f bi wo) (g wi bo)
  where f bi wo = rsf bi >>> rsf wo
        g wi bo = rsf wi >>> sf >>> rsf bo

par :: CSF a b -> CSF a c -> CSF a (b,c)
sf1 `par` sf2 = letW [] $ \wi bi -> letW [] $ \wo bo -> forkSF (f bi wo) (g wi bo)
  where f bi wo = proc a -> do
            () <- rsf bi -< a
            b <- sf1 -< a
            c <- brsf wo -< ()
            returnA -< (b,c)
        g wi bo = brsf wi >>> sf2 >>> rsf bo

parTo :: CSF a b -> CSF b () -> CSF a ()
sf1 `parTo` sf2 = letW [] $ \wi bi -> letW [] $ \wo bo -> forkSF (f bi) (forkSF (g wi bo) (h wo))
  where f bi = rsf bi
        g wi bo = brsf wi >>> sf1 >>> rsf bo
        h wo = brsf wo >>> sf2

async' :: CSF (Maybe a) (Maybe b) -> CSF (Maybe a) (Maybe b)
async' sf = async (arr collapse >>> sf) >>> arr collapse where
  collapse [] = Nothing
  collapse (Nothing:xs) = collapse xs
  collapse (x:_) = x

type UID = Integer

utracked :: UID -> CSF (Maybe a) (Maybe b) -> CSF (Maybe (a,UID)) (Maybe (b,UID))
utracked initUID sf = letW [initUID] $ \w b -> proc a -> do
  (uid:[]) <- rsf w -< ()
  let (a',uid') = case a of
        Nothing -> (Nothing, uid)
        Just (x,u) -> (Just x, u)
  b <- sf -< a'
  let b' = case b of
        Nothing -> Nothing
        Just x -> Just (x,uid')
  () <- rsf b -< uid'
  returnA -< b'

ctrl :: Show a => CSF () a -> CSF (Either b c) () -> CSF (Maybe a) (Maybe b)
     -> CSF (Maybe a) (Maybe c) -> CSF () ()
ctrl wh bh sf1 sf2 = letW [(0, Just ())] $ \w b -> proc () -> do
  ((uid,brsfCheck):[]) <- rsf w -< ()
  a <- case brsfCheck of
        Just () -> arr (:[]) <<< wh -< ()
        Nothing -> returnA -< []
--  a <- wh -< ()
  let (uid',a') = case a of
        [] -> (uid, Nothing)
        x:_ -> (uid+1, Just (x, uid+1))
  b1 <- async' (utracked 0 sf1) -< a'
  b2 <- async' (utracked 0 sf2) -< a'
  let b' = case (b1,b2) of
        (Just (y,u),_) | u == uid' -> Just (Left y)
        (_,Just (z,u)) | u == uid' -> Just (Right z)
        _ -> Nothing
  ret <- case b' of
        Just x -> arr (const $ Just ()) <<< bh -< x
        Nothing -> returnA -< Nothing
  () <- rsf b -< (uid', ret)
  returnA -< ()

spar :: Show a => CSF (Maybe a) (Maybe b) -> CSF (Maybe a) (Maybe c) -> CSF a (Either b c)
spar sf1 sf2 = letW [] $ \wi bi -> letW [] $ \wo bo -> forkSF (f bi wo) (g wi bo)
  where f bi wo = rsf bi >>> brsf wo
        g wi bo = ctrl (brsf wi) (rsf bo) sf1 sf2

