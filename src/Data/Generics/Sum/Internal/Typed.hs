{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Internal.Typed
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-field-type-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Internal.Typed
  ( GAsType (..)
  ) where

import Data.Kind
import GHC.Generics

import Data.Generics.Internal.Families
import Data.Generics.Product.Internal.HList
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism

-- |As 'AsType' but over generic representations as defined by "GHC.Generics".
class GAsType (f :: Type -> Type) (as :: Type) where
  _GTyped :: Prism (f x) (f x) as as

instance
  ( GIsList f f as as
  , ListTuple a a as as
  ) => GAsType (M1 C meta f) a where
  _GTyped = mIso . glist . tupled
  {-# INLINE[0] _GTyped #-}

instance GSumAsType (HasPartialTypeP (TupleToList a) l) l r a => GAsType (l :+: r) a where
  _GTyped = _GSumTyped @(HasPartialTypeP (TupleToList a) l)
  {-# INLINE[0] _GTyped #-}

instance GAsType f a => GAsType (M1 D meta f) a where
  _GTyped = mIso . _GTyped
  {-# INLINE[0] _GTyped #-}

class GSumAsType (contains :: Bool) l r (a :: Type) where
  _GSumTyped :: Prism ((l :+: r) x) ((l :+: r) x) a a

instance GAsType l a => GSumAsType 'True l r a where
  _GSumTyped = left . _GTyped
  {-# INLINE[0] _GSumTyped #-}

instance GAsType r a => GSumAsType 'False l r a where
  _GSumTyped = right . _GTyped
  {-# INLINE[0] _GSumTyped #-}
