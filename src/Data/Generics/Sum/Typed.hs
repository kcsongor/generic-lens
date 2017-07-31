{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Generics.Sum.Typed
  ( AsType (..)

  , GAsType (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.HList
import Data.Generics.Internal.Lens

import Data.Kind
import GHC.Generics
import GHC.TypeLits

class AsType a s where
  _Typed :: Prism' s a

instance
  ( Generic s
  , ErrorUnlessTrue a s (HasPartialTypeP a (Rep s))
  , GAsType (Rep s) a
  ) => AsType a s where

  _Typed = repIso . _GTyped

type family ErrorUnlessTrue (a :: Type) (s :: Type) (contains :: Bool) :: Constraint where
  ErrorUnlessTrue a s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a constructor whose field is of type "
        ':<>: 'ShowType a
        )

  ErrorUnlessTrue _ _ 'True
    = ()

class GAsType (f :: Type -> Type) a where
  _GTyped :: Prism' (f x) a

instance
  ( GCollectible f '[a]
  ) => GAsType (M1 C meta f) a where

  _GTyped = prism (M1 . gfromCollection . tupleToList) (Right . listToTuple @_ @'[a] . gtoCollection . unM1)

instance GSumAsType l r a (HasPartialTypeP a l) => GAsType (l :+: r) a where
  _GTyped = _GSumTyped @l @r @a @(HasPartialTypeP a l)

instance GAsType f a => GAsType (M1 D meta f) a where
  _GTyped = mIso . _GTyped

instance GAsType f a => GAsType (M1 S meta f) a where
  _GTyped = mIso . _GTyped

class GSumAsType l r a (contains :: Bool) where
  _GSumTyped :: Prism' ((l :+: r) x) a

instance GAsType l a => GSumAsType l r a 'True where
  _GSumTyped = left . _GTyped

instance GAsType r a => GSumAsType l r a 'False where
  _GSumTyped = right . _GTyped
