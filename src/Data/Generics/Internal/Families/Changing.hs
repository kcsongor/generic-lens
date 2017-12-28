{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Generics.Internal.Families.Changing
  ( Proxied
  , Infer
  ) where

import GHC.TypeLits (TypeError, ErrorMessage (..))
import Data.Type.Bool (If)

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

-- `P` can be used in place of any type parameter, which means that it can have
-- any kind, not just *, so a data type won't work.
-- (this caused https://github.com/kcsongor/generic-lens/issues/23)
-- Instead, we use a matchable type family to wrap any `k` - however, we can no longer directly
-- pattern match on `P`, as it's not a type constructor. But we can still take it apart as a polymorphic
-- application form. In order to distinguish between applications of P and other type constructors, we use a tag, `PTag`
-- to fake a type constructor.
data PTag = PTag
type family P :: Peano -> k -> PTag -> k

type Proxied t = Proxied' t 'Z

type family Proxied' (t :: k) (next :: Peano) :: k where
  Proxied' (t (a :: j) :: k) next = (Proxied' t ('S next)) (P next a 'PTag)
  Proxied' t _ = t

data Sub where
  Sub :: Peano -> k -> Sub

type family Unify (a :: k) (b :: k) :: [Sub] where
  Unify (a b) a' = If (IsPTag b) '[Bar (a b) a'] (Baz (a b) a')
  Unify a a = '[]
  Unify a b = TypeError
                ( 'Text "Couldn't match type "
                  ':<>: 'ShowType a
                  ':<>: 'Text " with "
                  ':<>: 'ShowType b
                )

type family Bar a b where
  Bar (p n _ 'PTag) a' = 'Sub n a'

type family Baz a b where
  Baz (a x) (b y) = Unify x y ++ Unify a b
  Baz a a = '[]
  Baz a b = TypeError
               ( 'Text "Couldn't match type "
                 ':<>: 'ShowType a
                 ':<>: 'Text " with "
                 ':<>: 'ShowType b
               )

type family IsPTag (a :: k) :: Bool where
  IsPTag 'PTag = 'True
  IsPTag _ = 'False

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Infer (s :: *) (a' :: *) (b :: *) :: * where
  Infer s a' b
    = ReplaceArgs s (Unify a' b)

--------------------------------------------------------------------------------

data Peano = Z | S Peano

-- [TODO]: work this out
--
--type family ArgKind (t :: k) (pos :: Peano) :: * where
--  ArgKind (t (a :: k)) 'Z = k
--  ArgKind (t _) ('S pos) = ArgKind t pos
--
--type family ReplaceArg (t :: k) (pos :: Peano) (to :: ArgKind t pos) :: k where
--  ReplaceArg (t a) 'Z to = t to
--  ReplaceArg (t a) ('S pos) to = ReplaceArg t pos to a
--  ReplaceArg t _ _ = t

type family ReplaceArg (t :: k) (pos :: Peano) (to :: j) :: k where
  ReplaceArg (t a) 'Z to = t to
  ReplaceArg (t a) ('S pos) to = ReplaceArg t pos to a
  ReplaceArg t _ _ = t

type family ReplaceArgs (t :: k) (subs :: [Sub]) :: k where
  ReplaceArgs t '[] = t
  ReplaceArgs t ('Sub n arg ': ss) = ReplaceArgs (ReplaceArg t n arg) ss
