{-# LANGUAGE Arrows, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, TypeSynonymInstances, OverlappingInstances #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, DataKinds #-}

module CSF (
  CSF, runCSF, stepCSF, 
  forkSF, 
  --async, buffer, parTo, spar, 
  --Reexporting
  Resource (..), --BResource (..), 
  letW,
  RData, Whitehole, Blackhole
) where

import Control.Monad (ap)
import Control.Applicative
import Control.Concurrent.MonadIO

import Data.IORef
import Data.Maybe (isJust, listToMaybe, catMaybes)

import Control.Monad.Writer.Strict

import TypeSet
import Arrow
import SF
import MSF
import CFRP

-----------------------------------------------------------
---------------------- Process State ----------------------
-----------------------------------------------------------

-- A given iteration of a signal function process can be thought of like a 
-- transaction.  In some situations, processes need to be frozen or killed, 
-- but the iteration they are currently executing must either complete or 
-- rollback.  We implement this by allowing it to complete but ignoring its 
-- effects, making it essentially behave like a rollback (but with poorer 
-- performance -- maybe we can fix that later.

-- These types describe Process information.  PStatus is the status of a 
-- given process, and each process has a status kept in an MVar knows as 
-- the PController.  Processes may also have PChilds, from either forks 
-- or choice statements, and these are grouped in an IORef called 
-- PChildren.  A process State (PState) is the pair of the controller 
-- and the process's children.

data PStatus = Proceed | ShouldFreeze | ShouldSkip | Frozen (MVar ()) | Die
type PController = MVar PStatus
data PChild = PForkChild PState | PChoiceChild PChildren
type PChildren = IORef [PChild]
type PState = (PController, PChildren)

-- Constructs a new PChildren object
newPChildren :: IO PChildren
newPChildren = newIORef []

-- Construct a new PState object with the given PStatus in the PController
newPState :: PStatus -> IO PState
newPState status = do
  mvar <- newMVar status
  pc <- newPChildren
  return (mvar, pc)

-- This function applies the given function f to all children, grandchildren, 
-- etc. in the given PChildren.  That is, it acts on all PControllers 
-- recursively within the given PChildren.  It is guaranteed to always act 
-- and complete on parents before accessing and then moving on to children.
actOnAllChildren :: (PController -> IO ()) -> PChildren -> IO ()
actOnAllChildren = actOnChildrenHelper True

actOnChildren :: (PController -> IO ()) -> PChildren -> IO ()
actOnChildren = actOnChildrenHelper False

actOnChildrenHelper :: Bool -> (PController -> IO ()) -> PChildren -> IO ()
actOnChildrenHelper recurP f ref = do
  childs <- readIORef ref
  mapM_ act childs
 where
  act (PForkChild (pc, children)) = f pc >> actOnChildrenHelper recurP f children
  act (PChoiceChild children) = if recurP then actOnChildrenHelper recurP f children else return ()


-- These three functions are the functions to send to actOnChildren 
-- to awaken, freeze, or kill all children.

wakeProcess :: PController -> IO ()
wakeProcess mvar = do
  currentState <- takeMVar mvar
  case currentState of
    Proceed -> putMVar mvar Proceed
    ShouldFreeze -> putMVar mvar ShouldSkip
    ShouldSkip -> putMVar mvar ShouldSkip
    Frozen wait -> putMVar mvar Proceed >> putMVar wait ()
    Die -> putMVar mvar Die

freezeProcess :: PController -> IO ()
freezeProcess mvar = do
  currentState <- takeMVar mvar
  case currentState of
    Proceed -> putMVar mvar ShouldFreeze
    ShouldFreeze -> putMVar mvar ShouldFreeze
    ShouldSkip -> putMVar mvar ShouldFreeze
    Frozen wait -> putMVar mvar currentState
    Die -> putMVar mvar Die

killProcess :: PController -> IO ()
killProcess mvar = do
  _currentState <- takeMVar mvar
  putMVar mvar Die


-----------------------------------------------------------
------------------------- CMonad --------------------------
-----------------------------------------------------------

type CSF = MSF CMonad
-- CMonad is an extension to IO to allow tracking of wormhole data 
-- and process data.  It is essentially just a reader and writer monad, 
-- but we define it explicitly.
newtype CMonad a = CMonad {unC :: PState -> IO ([RData], a)}

-- Adds a PChild to the children of the PState in this CMonad
addPChild :: PChild -> CMonad ()
addPChild child = CMonad $ \(_,children) -> (atomicModifyIORef children $ \lst -> (child:lst,())) >> return ([],())

instance Functor CMonad where
  fmap f xs = xs >>= return . f

instance Applicative CMonad where
  pure = return
  (<*>) = ap

instance Monad CMonad where
  return a = CMonad $ const $ return ([],a)
  (CMonad c) >>= f = CMonad $ \ps -> do 
    (rds,  a) <- c ps
    (rds', b) <- (unC $ f a) ps
    return (rds++rds', b)

instance MonadWriter [RData] CMonad where
  tell w = CMonad $ const $ return (w,())
  listen (CMonad c) = CMonad $ \ps -> do
    (rds, a) <- c ps
    return (rds, (a, rds))
  pass (CMonad c) = CMonad $ \ps -> do
    (rds, (a, ww)) <- c ps
    return (ww rds, a)

instance MonadIO CMonad where
  liftIO a = CMonad $ const $ a >>= (\v -> return ([],v))

-- The ArrowChoice instance is overridden to allow arrow choice to freeze 
-- child threads of unchosen branches.
instance ArrowChoice CSF where
  left msf = initialAction act (\pc -> MSF (h msf pc)) where
    act = do
      pc <- liftIO newPChildren
      addPChild (PChoiceChild pc)
      return pc
    h msf choiceChildren x = 
      -- We are only allowed to actOnChildren here if we are in a Proceed state.
      -- Otherwise, we are in skip/freeze/die, and we have no authority.
      case x of
        Left x' -> CMonad $ \(ps,_pc) -> do
          status <- takeMVar ps --begin critical region
          case status of
            --Since grand*children may be supposed to be sleeping, only wake up direct children
            Proceed -> actOnChildren wakeProcess choiceChildren
            _ -> return ()
          putMVar ps status --end critical region
          (rdata, (y, msf')) <- unC (msfFun msf x') (ps,choiceChildren)
          return (rdata, (Left y, MSF (h msf' choiceChildren)))
        Right z -> CMonad $ \(ps,_pc) -> do
          status <- takeMVar ps --begin critical region
          case status of
            --Every thread in this branch should be frozen
            Proceed -> actOnAllChildren freezeProcess choiceChildren
            _ -> return ()
          putMVar ps status --end critical region
          return ([],(Right z, MSF (h msf choiceChildren)))
  f ||| g = f +++ g >>> arr untag
        where
          untag (Left x) = x
          untag (Right y) = y


-- Running and stepping through CSFs
runCSF :: a -> PState -> CSF r a b -> IO b
runCSF a (ps@(mvar, _)) f = run f where 
  run f = do 
    (rds, (y, f')) <- (unC $ msfFun f $ a) ps
    -- We read our MVar, which tells us how we should handle resource effects
    command <- takeMVar mvar --begin critical region
    case command of
      -- Only if we are GO for proceeding do we stepRData
      -- stepRData performs the Ft-Time transition
      -- TODO: We may need to add strictness points (evaluate) within rds to make sure 
      --       that this call is short - it should be performing effects 
      --       rather than computing values.
      Proceed -> stepRData rds >> putMVar mvar Proceed >> run f'
      ShouldFreeze -> do
        wait <- newEmptyMVar
        putMVar mvar $ Frozen wait
        takeMVar wait
        run f
      ShouldSkip -> putMVar mvar Proceed >> run f
      Frozen _ -> error "Impossible: Frozen in runCSF"
      Die -> putMVar mvar Die >> return y
    --end critical region

runCSF' = runCSF ()

stepCSF :: CSF r a b -> [a] -> IO [b]
stepCSF csf inp = newPState Proceed >>= stepCSF' csf inp where
  stepCSF' _ [] (_, children) = actOnAllChildren killProcess children >> return []
  stepCSF' (MSF f) (x:xs) ps = do 
    (rds, (y, f')) <- (unC $ f x) ps
    -- This next line performs the Ft-Time transition
    stepRData rds
    ys <- stepCSF' f' xs ps
    -- This next line kills all spawned threads
    return (y:ys)


-- CMonad specific version of forkSF (for ease of coding)
forkSF :: CSF r () () -> CSF r a a
forkSF = forkSF' $ \csf -> CMonad $ \(mvar, refs) -> do
  status <- takeMVar mvar --begin critical region
  newPS <- newPState status
  _tid <- forkIO $ runCSF' newPS csf
  atomicModifyIORef refs $ \lst -> (PForkChild newPS:lst,())
  putMVar mvar status --end critical region
  return ([], ())

-----------------------------------------------------------
---------------------- Constructions ----------------------
-----------------------------------------------------------

data Console
instance Resource Console String () where
  get _ = return ()
  put _ = putStr

myfun :: CSF '[Int] [a] a -> CSF '[Int] () ()
myfun sf = letW [] f
  where f w b = rsf w >>> sf >>> rsf b


myfun2 :: forall a. CSF '[] [a] a -> CSF '[] () [a]
myfun2 sf = letW [] f where
--  f :: forall rw1 rb1. (Resource rw1 () [a]) => rw1 -> rb1 -> CSF '[rw1] () [a]
  f w1 b1 = letW [] g where
--    g :: forall rw2 rb2. (Resource rw2 () [a], ElemOf rw2 '[rw1] ~ False)
--      => rw2 -> rb2 -> CSF '[rw2,rw1] () [a]
    g w2 b2 = rsf w2 >>> arr (const ()) >>> rsf w1


myf :: forall rw1 rb1 a. (Resource rw1 () [a])
    => rw1 -> rb1 -> CSF '[rw1] () [a]
myf w1 b1 = letW [] (myg w1)

--myg :: forall rw1 rw2 rb2 a. (Resource rw1 () [a], Resource rw2 () [a])
--    => rw1 -> rw2 -> rb2 -> CSF '[rw2,rw1] () [a]
myg w1 w2 b2 = rsf w2 >>> arr (const undefined) >>> rsf w1

async :: CSF '[Int] [a] b -> CSF '[Int] a [b]
async sf = letW [] $ \wi bi -> letW [] $ \wo bo -> forkSF (g wi bo) >>> (f bi wo)
  where f bi wo = rsf bi >>> rsf wo
        g wi bo = rsf wi >>> sf >>> rsf bo

parTo :: CSF '[Int] a (Maybe b) -> CSF '[Char] (Maybe b) () -> CSF '[Char,Int] a ()
sf1 `parTo` sf2 = letW [] $ \w b -> forkSF (g w) >>> f b
  where f b = sf1 >>> rsf b
        g w = buffer (rsf w) >>> sf2
        buffer = undefined :: CSF r a [Maybe b] -> CSF r a (Maybe b)

{-

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

-}
