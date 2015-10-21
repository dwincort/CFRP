{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, IncoherentInstances #-}


module TypeSet where
import Unsafe.Coerce

class LRemove (x :: *) (ys :: [*]) (zs :: [*]) | x ys -> zs where
  lremove :: x -> a ys b c -> a zs b c
instance LRemove x '[] '[] where
  lremove _ = unsafeCoerce
instance (ys ~ zs) => LRemove x (x ': ys) zs where
  lremove _ = unsafeCoerce
instance (LRemove x ys ys', (y ': ys') ~ zs) => LRemove x (y ': ys) zs where
  lremove _ = unsafeCoerce


class TypeEq (x :: k) (y :: k) (b :: Bool) | x y -> b
instance (True ~ b)  => TypeEq x x b
instance (False ~ b) => TypeEq x y b

type family IfThenElse (b :: Bool) (x :: k) (y :: k) :: k where
  IfThenElse True x y = x
  IfThenElse False x y = y


class ElemOf (x :: *) (ys :: [*]) (b :: Bool) | x ys -> b
instance ElemOf x '[] False
instance (TypeEq x y b, ElemOf x ys z, r ~ IfThenElse b True z) => ElemOf x (y ': ys) r

class Union (xs :: [*]) (ys :: [*]) (zs :: [*]) | xs ys -> zs
instance Union '[] '[] '[]
instance Union '[] ys ys
instance Union xs '[] xs
instance (ElemOf x ys b, res ~ IfThenElse b ys (x ': ys), Union xs res zs) => Union (x ': xs) ys zs

class Disjoint (xs :: [*]) (ys :: [*])
instance Disjoint '[] '[]
instance Disjoint '[] ys
instance Disjoint xs '[]
instance (ElemOf x ys False, Disjoint xs ys) => Disjoint (x ': xs) ys

class Join (xs :: [*]) (ys :: [*]) (zs :: [*]) | xs ys -> zs
instance (Disjoint xs ys, Union xs ys zs) => Join xs ys zs


{-
type family OR (a :: Bool) (b :: Bool) :: Bool where
  OR True  b  = True
  OR a True  = True
  OR False b = b
  OR a False = a


type family TypeEq (a :: k) (b :: k) :: Bool where
  TypeEq a a = True
  TypeEq a b = False

type family IfThenElse (b :: Bool) (x :: k) (y :: k) :: k where
  IfThenElse True x y = x
  IfThenElse False x y = y

type family ElemOf (x :: *) (ys :: [*]) :: Bool where
  ElemOf x '[] = False
--  ElemOf x (x ': ys) = True
--  ElemOf x (y ': ys) = ElemOf x ys
  ElemOf x (y ': ys) = (TypeEq x y) `OR` (ElemOf x ys)


type family Union (xs :: [*]) (ys :: [*]) :: [*] where
  Union '[] '[] = '[]
  Union '[] ys = ys
  Union xs '[] = xs
  Union (x ': xs) ys = (x ': Union xs ys)

--type family Disjoint

class Disjoint (xs :: [*]) (ys :: [*])
instance Disjoint '[] '[]
instance Disjoint '[] ys
instance Disjoint xs '[]
instance (False ~ ElemOf x ys, Disjoint xs ys) => Disjoint (x ': xs) ys

--class Disjoint (xs :: [*]) (ys :: [*])
--instance Disjoint '[] ys
--instance (False ~ ElemOf x ys, Disjoint xs ys) => Disjoint (x ': xs) ys
--instance Disjoint xs '[]
--instance (False ~ ElemOf y xs, Disjoint xs ys) => Disjoint xs (y ': ys)


-}






