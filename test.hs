{-# LANGUAGE Arrows, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, FlexibleContexts #-}
import Control.Arrow
import CSF


-- Toy resource instances for keyboard and console

-- Keyboard would be better if its output stream were [Char] and it was 
-- more of a buffered keyboard that didn't block.
--data Keyboard
--instance Resource Keyboard () Char where
--  get _ = getChar
--  put _ _ = return ()

data Console
instance Resource Console String () where
  get _ = return ()
  put _ = putStr


-- A signal function that waits ten steps before outputting an event and
-- is reset on an input event.
-- The console output was added to force OS thread switching so the test 
-- results are more interesting.
countToTen :: CSF (Maybe a) (Maybe String)
countToTen = letW [10] $ \wh bh -> proc ma -> do
    [c] <- rsf wh -< ()
    let c' = maybe (c-1) (const 10) ma
    rsf (undefined :: Console) -< show c' 
    () <- rsf bh -< c'
    returnA -< if c' == 0 then Just "Ten" else Nothing

-- A signal function that waits n steps before outputting an event where 
-- n is reset for every new input event.
countToN :: CSF (Maybe Int) (Maybe String)
countToN = letW [-1] $ \wh bh -> proc ma -> do
    [c] <- rsf wh -< ()
    let c' = maybe (c-1) id ma
    rsf (undefined :: Console) -< show c'
    () <- rsf bh -< c'
    returnA -< if c' == 0 then Just "N" else Nothing


runTest = stepCSF

test0 :: forall w b t. (Resource w () [t], Resource b t ()) => w -> b -> CSF t t
test0 wh bh = rsf bh >>> rsf wh >>> arr head

-- 1) Testing wormholes functioning as a delay operator
-- runTest test1 [1..5]
test1 = letW [0] $ test0
-- 2) Testing brsf updating
-- runTest test2 [1..5]
--test2 :: CSF Int Int
--test2 = letW [0] $ \wh bh -> rsf bh >>> brsf wh >>> arr head
-- 3) Testing Console output and the use of wormholes to keep state
-- runTest test3 (replicate 10 ())
test3 :: CSF () ()
test3 = letW ["a"] $ \wh bh -> rsf wh >>> arr head >>> (rsf (undefined :: Console)) &&& rsf bh >>> arr fst
-- 4) Testing proper asynchrony of fork (running test4 multiple times 
--    will yield a different ordering of output to the console).  Note 
--    that test4 is technically resource unsafe.
-- runTest test4 (map show [1..20])
test4 = rsf (undefined :: Console) >>> forkSF test3
-- 5) Testing spar
-- (runTest test5 $ Just 200 : (replicate 2000 Nothing)) >>= (return . catMaybes)
-- (runTest test5 $ Just 2   : (replicate 2000 Nothing)) >>= (return . catMaybes)
test5 = spar countToTen countToN
