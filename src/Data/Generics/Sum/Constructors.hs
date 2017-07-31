{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Generics.Sum.Constructors
  ( AsConstructor (..)

  , GAsConstructor (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.HList
import Data.Generics.Internal.Lens

import Data.Kind
import GHC.Generics
import GHC.TypeLits

class AsConstructor (ctor :: Symbol) a s | s ctor -> a where
  _Ctor :: Prism' s a

instance
  ( Generic s
  , ErrorUnlessTrue ctor s (HasCtorP ctor (Rep s))
  , GAsConstructor ctor (Rep s) a
  ) => AsConstructor ctor a s where

  _Ctor = repIso . _GCtor @ctor

type family ErrorUnlessTrue (ctor :: Symbol) (s :: Type) (contains :: Bool) :: Constraint where
  ErrorUnlessTrue ctor s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a constructor named "
        ':<>: 'ShowType ctor
        )

  ErrorUnlessTrue _ _ 'True
    = ()

class GAsConstructor (ctor :: Symbol) (f :: Type -> Type) a | ctor f -> a where
  _GCtor :: Prism' (f x) a

instance
  ( GCollectible f as
  , ListTuple a as
  ) => GAsConstructor ctor (M1 C ('MetaCons ctor fixity fields) f) a where

  _GCtor = prism (M1 . gfromCollection . tupleToList) (Right . listToTuple @_ @as . gtoCollection . unM1)

instance GSumAsConstructor ctor l r a (HasCtorP ctor l) => GAsConstructor ctor (l :+: r) a where
  _GCtor = _GSumCtor @ctor @l @r @a @(HasCtorP ctor l)

instance GAsConstructor ctor f a => GAsConstructor ctor (M1 D meta f) a where
  _GCtor = mIso . _GCtor @ctor

instance GAsConstructor ctor f a => GAsConstructor ctor (M1 S meta f) a where
  _GCtor = mIso . _GCtor @ctor

class GSumAsConstructor (ctor :: Symbol) l r a (contains :: Bool) | ctor l r contains -> a where
  _GSumCtor :: Prism' ((l :+: r) x) a

instance GAsConstructor ctor l a => GSumAsConstructor ctor l r a 'True where
  _GSumCtor = left . _GCtor @ctor

instance GAsConstructor ctor r a => GSumAsConstructor ctor l r a 'False where
  _GSumCtor = right . _GCtor @ctor
