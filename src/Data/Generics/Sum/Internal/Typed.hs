{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Internal.Typed
-- Copyright   :  (C) 2017 Csongor Kiss
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
import Data.Generics.Internal.HList
import Data.Generics.Internal.Lens

-- |As 'AsType' but over generic representations as defined by "GHC.Generics".
class GAsType (f :: Type -> Type) a where
  _GTyped :: Prism' (f x) a
  _GTyped = prism ginjectTyped gprojectTyped

  ginjectTyped  :: a -> f x
  gprojectTyped :: f x -> Either (f x) a

instance
  ( GCollectible f as
  , ListTuple a as
  ) => GAsType (M1 C meta f) a where

  ginjectTyped
    = M1 . gfromCollection . tupleToList
  gprojectTyped
    = Right . listToTuple . gtoCollection . unM1

instance GSumAsType (HasPartialTypeTupleP a l) l r a => GAsType (l :+: r) a where
  ginjectTyped
    = ginjectSumTyped @(HasPartialTypeTupleP a l) @l @r @a
  gprojectTyped
    = gprojectSumTyped @(HasPartialTypeTupleP a l) @l @r @a

instance GAsType f a => GAsType (M1 D meta f) a where
  ginjectTyped
    = M1 . ginjectTyped
  gprojectTyped
    = either (Left . M1) Right . gprojectTyped . unM1

class GSumAsType (contains :: Bool) l r a where
  _GSumTyped :: Prism' ((l :+: r) x) a
  _GSumTyped = prism (ginjectSumTyped  @contains) (gprojectSumTyped @contains)

  ginjectSumTyped  :: a -> (l :+: r) x
  gprojectSumTyped :: (l :+: r) x -> Either ((l :+: r) x) a

instance GAsType l a => GSumAsType 'True l r a where
  ginjectSumTyped
    = L1 . ginjectTyped
  gprojectSumTyped x
    = case x of
        L1 l -> either (Left . L1) Right (gprojectTyped l)
        R1 _ -> Left x

instance GAsType r a => GSumAsType 'False l r a where
  ginjectSumTyped
    = R1 . ginjectTyped
  gprojectSumTyped x
    = case x of
        R1 r -> either (Left . R1) Right (gprojectTyped r)
        L1 _ -> Left x
