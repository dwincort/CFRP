{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, IncoherentInstances #-}


module TypeSet where
import Unsafe.Coerce

data TSet a = U (TSet a) (TSet a)
            | S a
            | E

type family OR (a :: Bool) (b :: Bool) :: Bool where
  OR True  b  = True
  OR a True  = True
  OR False False = False


type family TypeEq (a :: k) (b :: k) :: Bool where
  TypeEq a a = True
  TypeEq a b = False

type family IfThenElse (b :: Bool) (x :: k) (y :: k) :: k where
  IfThenElse True x y = x
  IfThenElse False x y = y

type family ElemOf (x :: *) (ys :: TSet *) :: Bool where
  ElemOf x E = False
  ElemOf x (S x) = True
  ElemOf x (S y) = False
  ElemOf x (U ys zs) = OR (ElemOf x ys) (ElemOf x zs)


-- let's see if I can rewrite union so that it uses append???
type family Union (xs :: TSet *) (zs :: TSet *) :: TSet * where
  Union xs E = xs
  Union E zs = zs
  Union (S a) zs = IfThenElse (ElemOf a zs) zs (S a `U` zs)
  Union (U xs ys) zs = Union xs (Union ys zs)

class Disjoint (xs :: TSet *) (zs :: TSet *)
instance Disjoint E zs
instance Disjoint xs E
instance (False ~ ElemOf x zs) => Disjoint (S x) zs
instance (Disjoint xs zs, Disjoint ys zs) => Disjoint (U xs ys) zs

class LRemove (x :: *) (ys :: TSet *) (zs :: TSet *) | x ys -> zs where
  lremove :: x -> a ys b c -> a zs b c
instance LRemove x E E where
  lremove _ = unsafeCoerce
instance zs ~ E => LRemove x (S x) zs where
  lremove _ = unsafeCoerce
instance zs ~ (S y) => LRemove x (S y) zs where
  lremove _ = unsafeCoerce
instance (LRemove x ys zs, LRemove x ys' zs', zs'' ~ Union zs zs') => LRemove x (U ys ys') zs'' where
  lremove _ = unsafeCoerce








