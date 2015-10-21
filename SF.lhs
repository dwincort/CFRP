
> {-# LANGUAGE DataKinds, TypeOperators, KindSignatures #-}
> module SF where

> import Arrow
> import TypeSet

> newtype SF (r :: [*]) a b = SF { runSF :: (a -> (b, SF r a b)) }


> instance Category SF where
>   iden = SF h where h x = (x, SF h)
>   g <<< f = SF (h f g)
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

> instance ArrowChoice SF where
>    left sf = SF (g sf) where 
>          g f x = case x of
>                    Left a -> let (y, f') = runSF f a in f' `seq` (Left y, SF (g f'))
>                    Right b -> (Right b, SF (g f))
>    sf1 ||| sf2 = SF (h sf1 sf2) where
>        h f g x = case x of 
>            Left b  -> let (d, f') = runSF f b in f' `seq` (d, SF (h f' g))
>            Right c -> let (d, g') = runSF g c in g' `seq` (d, SF (h f g'))

> run :: SF r a b -> [a] -> [b]
> run (SF f) (x:xs) =
>   let (y, f') = f x 
>   in y `seq` f' `seq` (y : run f' xs)

> unfold :: SF r () a -> [a]
> unfold = flip run inp
>   where inp = () : inp

