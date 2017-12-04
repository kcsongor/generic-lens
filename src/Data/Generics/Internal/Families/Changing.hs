{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generics.Internal.Families.Changing where

import GHC.TypeLits (Nat, type (+))

{-
  Note [Changing type parameters]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  To get good type inference for type-changing lenses, we want to be able
  map the field's type back to the type argument it corresponds to. This way,
  when the field is changed, we know what the result type of the structure is
  going to be.

  However, for a given type @t@, its representation @Rep t@ forgets which types
  in the structure came from type variables, and which didn't. An @Int@ that
  results from the instantiation of the type paremeter and an @Int@ that was
  monomorphically specified in the structure are indistinguishable.

  The solution is to replace the type arguments in the type with unique
  proxies, like: @T a b@ -> @T (P 1 a) (P 0 b)@. This way, if looking up
  a field's type yields something of shape @P _ _@, we know it came from a type
  parameter, and also know which.

  If the field's type is a proxy, then its type is allowed to change, otherwise
  not. This also allows us to satisfy the functional dependency @s field b -> t@.
  If after doing the conversion on @s@, @field@'s type is @(P _ a), then @t@ is
  @s[b/a]@, otherwise @t ~ s@ and @b ~ a@.
-}
data P (i :: Nat) a

type Proxied t = Proxied' t 0

type family Proxied' (t :: k) (next :: Nat) :: k where
  Proxied' (t a :: k) next = (Proxied' t (next + 1)) (P next a)
  Proxied' t _ = t

type family UnProxied (t :: k) :: k where
  UnProxied (P _ a) = a
  UnProxied (t (P _ a) :: k) = UnProxied t a
  UnProxied t = t

type family Change (t :: k) (target :: Nat) (to :: j) :: k where
  Change (P target _) target to = (P target to)
  Change (t (P target _) :: k) target to = t (P target to)
  Change (t a :: k) target to = Change t target to a
  Change t _ _ = t

-- TODO: document this
data Skolem

type family Generalise (t :: k) (w :: *) :: k where
  Generalise Skolem w = w
  Generalise (t Skolem :: k) w = t w
  Generalise (t a) w = (Generalise t w) a
  Generalise t _ = t

type family UnApply (a :: k) :: [*] where
  UnApply (f x) = x ': UnApply f
  UnApply x     = '[]

type family Unify a b :: [(*, *)] where
  Unify (P n a') a = '[ '(P n a', a)]
  Unify a b = Zip (UnApply a) (UnApply b)

type family PSub (subs :: [(*, *)]) :: [(Nat, *)] where
  PSub '[] = '[]
  PSub ('(P n _, b) ': xs) = '(n, b) ': PSub xs
  PSub (_ ': xs) = PSub xs

type family Zip (xs :: [k]) (ys :: [l]) :: [(k, l)] where
  Zip '[] '[] = '[]
  Zip (x ': xs) (y ': ys) = '(x, y) ': Zip xs ys

type family IfEq a b t f where
  IfEq a a t _ = t
  IfEq _ _ _ f = f

type family Infer (s :: *) (a' :: *) (a :: *) (w :: *) :: (*, *) where
  Infer s' a' a w
    = Infer' s' a' (PSub (Unify a' a)) w

type family Infer' (s :: *) (a' :: *) (subs :: [(Nat, k)]) w :: (*, *) where
  Infer' s' a' '[ '(p, _)] w
    = '(Generalise (UnProxied (Change s' p Skolem)) w, Generalise (UnProxied (Change a' p Skolem)) w)
  Infer' s' a' _ _
    = '(UnProxied s', UnProxied a')

type family PickTv (a :: k) (b :: k) :: * where
  PickTv (P _ _) b = b
  PickTv (f (P _ _)) (g b) = b
  PickTv (f a) (g b) = PickTv f g
