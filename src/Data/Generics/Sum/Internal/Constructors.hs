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
-- Copyright   :  (C) 2017 Csongor Kiss
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
import Data.Generics.Product.Internal.List
import Data.Generics.Internal.Lens

import GHC.Generics
import GHC.TypeLits (Symbol)
import Data.Kind

-- |As 'AsConstructor' but over generic representations as defined by
--  "GHC.Generics".
class GAsConstructor (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b where
  _GCtor :: Prism (s x) (t x) a b

type GAsConstructor' ctor s a = GAsConstructor ctor s s a a

instance
  ( GIsList Type f f as as
  , GIsList Type g g bs bs
  , ListTuple a as
  , ListTuple b bs
  ) => GAsConstructor ctor (M1 C ('MetaCons ctor fixity fields) f) (M1 C ('MetaCons ctor fixity fields) g) a b where

  _GCtor = prism (M1 . (^. fromIso (glist @Type)) . tupleToList) (Right . listToTuple . (^. glist @Type) . unM1)

instance GSumAsConstructor ctor (HasCtorP ctor l) l r l' r' a b => GAsConstructor ctor (l :+: r) (l' :+: r') a b where
  _GCtor = _GSumCtor @ctor @(HasCtorP ctor l)

instance GAsConstructor ctor f f' a b => GAsConstructor ctor (M1 D meta f) (M1 D meta f') a b where
  _GCtor = mIso . _GCtor @ctor

class GSumAsConstructor (ctor :: Symbol) (contains :: Bool) l r l' r' a b | ctor l r -> a, ctor l' r' -> b where
  _GSumCtor :: Prism ((l :+: r) x) ((l' :+: r') x) a b

instance GAsConstructor ctor l l' a b => GSumAsConstructor ctor 'True l r l' r a b where
  _GSumCtor = left . _GCtor @ctor

instance GAsConstructor ctor r r' a b => GSumAsConstructor ctor 'False l r l r' a b where
  _GSumCtor = right . _GCtor @ctor
