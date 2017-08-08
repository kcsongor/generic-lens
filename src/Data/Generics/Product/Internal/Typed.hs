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
-- Module      :  Data.Generics.Product.Internal.Typed
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive record field getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Typed
  ( GHasType (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics

-- |As 'HasType' but over generic representations as defined by
--  "GHC.Generics".
class GHasType (f :: Type -> Type) a where
  gtyped :: Lens' (f x) a

instance GProductHasType l r a (HasTotalTypeP a l)
      => GHasType (l :*: r) a where

  gtyped = gproductTyped @_ @_ @_ @(HasTotalTypeP a l)

instance (GHasType l a, GHasType r a) => GHasType (l :+: r) a where
  gtyped = combine (gtyped @l) (gtyped @r)

instance GHasType (K1 R a) a where
  gtyped f (K1 x) = fmap K1 (f x)

instance GHasType f a => GHasType (M1 m meta f) a where
  gtyped = mIso . gtyped

class GProductHasType l r a (contains :: Bool) where
  gproductTyped :: Lens' ((l :*: r) x) a

instance GHasType l a => GProductHasType l r a 'True where
  gproductTyped = first . gtyped

instance GHasType r a => GProductHasType l r a 'False where
  gproductTyped = second . gtyped
