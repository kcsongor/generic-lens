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

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.Fields
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive record field getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Fields
  ( GHasField (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics
import GHC.TypeLits (Symbol)

-- |As 'HasField' but over generic representations as defined by
--  "GHC.Generics".
class GHasField (field :: Symbol) (f :: Type -> Type) a | field f -> a where
  gfield :: Lens' (f x) a

instance GProductHasField field l r a (HasTotalFieldP field l)
      => GHasField field (l :*: r) a where

  gfield = gproductField @field @_ @_ @_ @(HasTotalFieldP field l)

instance (GHasField field l a, GHasField field r a)
      =>  GHasField field (l :+: r) a where

  gfield = combine (gfield @field @l) (gfield @field @r)

instance GHasField field (K1 R a) a where
  gfield f (K1 x) = fmap K1 (f x)

instance GHasField field (S1 ('MetaSel ('Just field) upkd str infstr) (Rec0 a)) a where
  gfield = mIso . gfield @field

instance GHasField field f a => GHasField field (M1 D meta f) a where
  gfield = mIso . gfield @field

instance GHasField field f a => GHasField field (M1 C meta f) a where
  gfield = mIso . gfield @field

class GProductHasField (field :: Symbol) l r a (left :: Bool) | left field l r -> a where
  gproductField :: Lens' ((l :*: r) x) a

instance GHasField field l a => GProductHasField field l r a 'True where
  gproductField = first . gfield @field

instance GHasField field r a => GProductHasField field l r a 'False where
  gproductField = second . gfield @field
