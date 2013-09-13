> {-# LANGUAGE CPP #-}

> module SF where

#if __GLASGOW_HASKELL__ >= 610
> import Control.Category
> import Prelude hiding ((.), init, exp)
#else
> import Prelude hiding (init, exp)
#endif

> import Control.Arrow


> newtype SF a b = SF { runSF :: (a -> (b, SF a b)) }


#if __GLASGOW_HASKELL__ >= 610
> instance Category SF where
>   id = SF h where h x = (x, SF h)
>   g . f = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f x
>             (z, g') = runSF g y
>         in f' `seq` g' `seq` (z, SF (h f' g'))

> instance Arrow SF where
>   arr f = g
>     where g = SF (\x -> (f x, g))
>   first f = SF (g f)
>     where
>       g f (x, z) = f' `seq` ((y, z), SF (g f'))
>         where (y, f') = runSF f x
>   f &&& g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f x
>             (z, g') = runSF g x 
>         in ((y, z), SF (h f' g'))
>   f *** g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f (fst x)
>             (z, g') = runSF g (snd x) 
>         in ((y, z), SF (h f' g'))
#else
> instance Arrow SF where
>   arr f = g
>     where g = SF (\x -> (f x, g))
>   f >>> g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f x
>             (z, g') = runSF g y
>         in (z, SF (h f' g'))
>   first f = SF (g f)
>     where
>       g f (x, z) = ((y, z), SF (g f'))
>         where (y, f') = runSF f x
>   f &&& g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f x
>             (z, g') = runSF g x 
>         in ((y, z), SF (h f' g'))
>   f *** g = SF (h f g)
>     where
>       h f g x =
>         let (y, f') = runSF f (fst x)
>             (z, g') = runSF g (snd x) 
>         in ((y, z), SF (h f' g'))
#endif

> instance ArrowChoice SF where
>    left sf = SF (g sf)
>        where 
>          g f x = case x of
>                    Left a -> let (y, f') = runSF f a in f' `seq` (Left y, SF (g f'))
>                    Right b -> (Right b, SF (g f))

> run :: SF a b -> [a] -> [b]
> run (SF f) (x:xs) =
>   let (y, f') = f x 
>   in y `seq` f' `seq` (y : run f' xs)

> unfold :: SF () a -> [a]
> unfold = flip run inp
>   where inp = () : inp

