{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE UnsaturatedTypeFamilies  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.Families.Has
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.Families.Has
  ( HasTotal
  , Pos
  , HasPartialTypeP
  , HasCtorP
  , GTypes
  ) where

import Data.Type.Bool     (type (||))
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits (Nat)
import Data.Kind (Type)
import Type.Prelude hiding (All)

type family Both (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Both ('Just a) ('Just a) = 'Just a
  Both _ _ = 'Nothing

type family Alt (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Alt ('Just a) _ = 'Just a
  Alt _ b = b

data Pos (p :: Nat)

type family MatchField (field :: k) (r :: Type -> Type) :: Maybe Type where
  MatchField field (S1 ('MetaSel ('Just field) _ _ _) (K1 _ t)) = 'Just t
  MatchField t     (S1 _ (K1 _ t))                              = 'Just t
  MatchField pos   (S1 _ (K1 (Pos pos) t))                      = 'Just t
  MatchField _ _                                                = 'Nothing

type family MatchC1 (r :: Type -> Type) :: Maybe (Type -> Type) where
  MatchC1 (C1 a f) = 'Just f
  MatchC1 _ = 'Nothing

type HasTotal field
  = Foldl1 Both . Map (Foldl Alt 'Nothing . (Gmap (MatchField field))) . ConstructorsOf
type ConstructorsOf    = GmapMaybe MatchC1
type HasPartialTypeP a = Elem a . Map GTypes . ConstructorsOf
type HasCtorP ctor     = Foldl (||) 'False . Gmap ((==) ('MetaCons ctor))
type GTypes            = GmapMaybe GType

type family GType (r :: Type -> Type) :: Maybe Type where
  GType (S1 _ (K1 _ t)) = 'Just t
  GType _ = 'Nothing
