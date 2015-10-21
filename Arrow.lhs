\begin{code}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DataKinds, TypeOperators #-}

module Arrow where
import TypeSet
\end{code}



\begin{code}
class Category (cat :: [*] -> * -> * -> *) where
    iden  :: cat '[] a a
    (>>>) :: Join r1 r2 r3 => cat r1 a b -> cat r2 b c -> cat r3 a c
    (<<<) :: Join r1 r2 r3 => cat r2 b c -> cat r1 a b -> cat r3 a c
    f <<< g = g >>> f
    f >>> g = g <<< f

class Category a => Arrow a where
    arr    :: (b -> c) -> a '[] b c
    first  :: a r b c -> a r (b, d) (c, d)
    second :: a r b c -> a r (d, b) (d, c)
    second f = arr swap >>> first f >>> arr swap
                 where   swap ~(x,y) = (y,x)
    (***)  :: Join r1 r2 r3 => a r1 b c -> a r2 b' c' -> a r3 (b,b') (c,c')
    f *** g = first f >>> second g
    (&&&)  :: Join r1 r2 r3 => a r1 b c -> a r2 b c' -> a r3 b (c,c')
    f &&& g = arr (\b -> (b,b)) >>> (f *** g)
\end{code}

The identity arrow, which plays the role of 'return' in arrow notation.
\begin{code}
returnA :: Arrow a => a '[] b b
returnA = arr id
\end{code}


What follows are some standard Arrow extensions: Loop, Choice, and Delay.
\begin{code}
class Arrow a => ArrowLoop a where
    loop :: a r (b, d) (c, d) -> a r b c

class Arrow a => ArrowChoice a where
    left  :: a r b c -> a r (Either b d) (Either c d)
    left  f = f +++ arr id
    right :: a r b c -> a r (Either d b) (Either d c)
    right f = arr id +++ f
    (+++) :: Union r1 r2 r3 => a r1 b c -> a r2 b' c' -> a r3 (Either b b') (Either c c')
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    (|||) :: Union r1 r2 r3 => a r1 b d -> a r2 c d -> a r3 (Either b c) d

class Arrow a => ArrowDelay a where
    delay :: b -> a '[] b b
\end{code}

Precomposition with a pure function.
\begin{code}
(^>>) :: Arrow a => (b -> c) -> a r c d -> a r b d
f ^>> a = arr f >>> a
\end{code}

Postcomposition with a pure function.
\begin{code}
(>>^) :: Arrow a => a r b c -> (c -> d) -> a r b d
a >>^ f = a >>> arr f
\end{code}

Precomposition with a pure function (right-to-left variant).
\begin{code}
(<<^) :: Arrow a => a r c d -> (b -> c) -> a r b d
a <<^ f = a <<< arr f
\end{code}

Postcomposition with a pure function (right-to-left variant).
\begin{code}
(^<<) :: Arrow a => (c -> d) -> a r b c -> a r b d
f ^<< a = arr f <<< a
\end{code}


What follows are some experimental arrow extensions
\begin{code}
class (Monad m, Arrow a) => ArrowAction m a where
    initialAction :: m d -> (d -> a r b c) -> a r b c
\end{code}


