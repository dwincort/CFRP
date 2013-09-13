{-# LANGUAGE Arrows, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
import Control.Arrow
import CSF


-- Toy resource instances for keyboard and console
data Keyboard
instance Resource Keyboard () Char where
  process _ () = getChar

data Console
instance Resource Console String () where
  process _ = putStr


-- A signal function that waits ten steps before outputting an event and
-- is reset on an input event.
countToTen :: CSF (Maybe a) (Maybe ())
countToTen = letW [10] $ \wh bh -> proc ma -> do
    [c] <- rsf wh -< ()
    let c' = maybe (c-1) (const 10) ma
    rsf (undefined :: Console) -< show c'
    () <- rsf bh -< c'
    returnA -< if c' == 0 then Just () else Nothing

-- A signal function that waits n steps before outputting an event where 
-- n is reset for every new input event.
countToN :: CSF (Maybe Int) (Maybe ())
countToN = letW [-1] $ \wh bh -> proc ma -> do
    [c] <- rsf wh -< ()
    let c' = maybe (c-1) id ma
--    rsf (undefined :: Console) -< show c'
    () <- rsf bh -< c'
    returnA -< if c' == 0 then Just () else Nothing


runTest = stepCSF

-- 1) Testing wormholes functioning as a delay operator
-- runTest test1 [1..5]
test1 = letW [] $ \wh bh -> rsf bh >>> rsf wh
-- 2) Testing brsf updating
-- runTest test2 [1..5]
test2 = letW [] $ \wh bh -> rsf bh >>> brsf wh
-- 3) Testing Console output and the use of wormholes to keep state
-- runTest test3 (replicate 10 ())
test3 = letW ["a","b"] $ \wh bh -> brsf wh >>> (rsf (undefined :: Console)) &&& rsf bh >>> arr fst
-- 4) Testing proper asynchrony of fork (running test4 multiple times 
--    will yield a different ordering of output to the console).  Note 
--    that test4 is technically resource unsafe.
-- runTest test4 (map show [1..20])
test4 = rsf (undefined :: Console) >>> forkSF (arr id) test3
-- 5) Testing spar
-- runTest test5 [1,200,1,20,1]
test5 = spar countToTen countToN

