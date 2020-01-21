{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
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

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Internal.Constructors
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-name-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Internal.Constructors
  ( GAsConstructor (..)
  , GAsConstructor'
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Product.Internal.HList

import GHC.Generics
import GHC.TypeLits (Symbol)
import Data.Generics.Internal.Optic.Iso as Iso
import Data.Generics.Internal.Optic.Prism as Prism

-- |As 'AsConstructor' but over generic representations as defined by
--  "GHC.Generics".
class GAsConstructor (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b where
  _GCtor :: Prism (s x) (t x) a b

type GAsConstructor' ctor s a = GAsConstructor ctor s s a a

instance
  ( GIsList f f as as
  , GIsList g g bs bs
  , ListTuple a as
  , ListTuple b bs
  ) => GAsConstructor ctor (M1 C ('MetaCons ctor fixity fields) f) (M1 C ('MetaCons ctor fixity fields) g) a b where

  _GCtor = prism (Iso.view (fromIso (mIso Iso.% glist Iso.% tupled))) (Right . Iso.view (mIso Iso.% glist Iso.% tupled))
  {-# INLINE[0] _GCtor #-}

instance GSumAsConstructor ctor (HasCtorP ctor l) l r l' r' a b => GAsConstructor ctor (l :+: r) (l' :+: r') a b where
  _GCtor = _GSumCtor @ctor @(HasCtorP ctor l)
  {-# INLINE[0] _GCtor #-}

instance GAsConstructor ctor f f' a b => GAsConstructor ctor (M1 D meta f) (M1 D meta f') a b where
  _GCtor = iso2prism mIso Prism.% _GCtor @ctor
  {-# INLINE[0] _GCtor #-}

class GSumAsConstructor (ctor :: Symbol) (contains :: Bool) l r l' r' a b | ctor l r -> a, ctor l' r' -> b where
  _GSumCtor :: Prism ((l :+: r) x) ((l' :+: r') x) a b

instance GAsConstructor ctor l l' a b => GSumAsConstructor ctor 'True l r l' r a b where
  _GSumCtor = left Prism.% _GCtor @ctor
  {-# INLINE[0] _GSumCtor #-}

instance GAsConstructor ctor r r' a b => GSumAsConstructor ctor 'False l r l r' a b where
  _GSumCtor = right Prism.% _GCtor @ctor
  {-# INLINE[0] _GSumCtor #-}
