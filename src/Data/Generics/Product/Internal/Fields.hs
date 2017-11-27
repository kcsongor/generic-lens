{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
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
  , GHasField'
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics
import GHC.TypeLits (Symbol)

-- |As 'HasField' but over generic representations as defined by
--  "GHC.Generics".
class GHasField (field :: Symbol) (s :: Type -> Type) (t :: Type -> Type) a b | s field -> a, t field -> b where
  gfield :: Lens (s x) (t x) a b

type GHasField' field s a = GHasField field s s a a

instance GProductHasField field l r l' r' a b (HasTotalFieldP field l)
      => GHasField field (l :*: r) (l' :*: r') a b where

  gfield = gproductField @field @_ @_ @_ @_ @_ @_ @(HasTotalFieldP field l)

instance (GHasField field r r a a, GHasField field l l a a, GHasField field l l' a b, GHasField field r r' a b)
      =>  GHasField field (l :+: r) (l' :+: r') a b where

  gfield f (L1 s) = fmap (\a -> L1 (set (gfield @field) a s)) (f (s ^. gfield @field))
  gfield f (R1 t) = fmap (\a -> R1 (set (gfield @field) a t)) (f (t ^. gfield @field))

instance GHasField field (K1 R a) (K1 R b) a b where
  gfield f (K1 x) = fmap K1 (f x)

instance GHasField field (S1 ('MetaSel ('Just field) upkd str infstr) (Rec0 a)) (S1 ('MetaSel ('Just field) upkd str infstr) (Rec0 b)) a b where
  gfield = mLens . gfield @field

instance (Functor g, GHasField field f g a b) => GHasField field (M1 D meta f) (M1 D meta g) a b where
  gfield = mLens . gfield @field

instance GHasField field f g a b => GHasField field (M1 C meta f) (M1 C meta g) a b where
  gfield = mLens . gfield @field

class GProductHasField (field :: Symbol) l r l' r' a b (left :: Bool) | field l r -> a, field l' r' -> b where
  gproductField :: Lens ((l :*: r) x) ((l' :*: r') x) a b

instance GHasField field l l' a b => GProductHasField field l r l' r a b 'True where
  gproductField = first . gfield @field

instance GHasField field r r' a b => GProductHasField field l r l r' a b 'False where
  gproductField = second . gfield @field
