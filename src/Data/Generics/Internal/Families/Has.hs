{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  ( HasTotalFieldP
  , HasTotalTypeP
  , HasTotalPositionP
  , Pos
  , HasPartialTypeP
  , HasCtorP
  , GTypes
  ) where

import Data.Type.Bool     (type (||))
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits (Symbol, Nat)
import Data.Kind (Type)

import Data.Generics.Product.Internal.HList

-- Note: these could be factored out into a single traversal

type family Both (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Both ('Just a) ('Just a) = 'Just a

type family Alt (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Alt ('Just a) _ = 'Just a
  Alt _ b = b

type family HasTotalFieldP (field :: Symbol) f :: Maybe Type where
  HasTotalFieldP field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'Just t
  HasTotalFieldP field (l :*: r)
    = Alt (HasTotalFieldP field l) (HasTotalFieldP field r)
  HasTotalFieldP field (l :+: r)
    = Both (HasTotalFieldP field l) (HasTotalFieldP field r)
  HasTotalFieldP field (S1 _ _)
    = 'Nothing
  HasTotalFieldP field (C1 _ f)
    = HasTotalFieldP field f
  HasTotalFieldP field (D1 _ f)
    = HasTotalFieldP field f
  HasTotalFieldP field (K1 _ _)
    = 'Nothing
  HasTotalFieldP field U1
    = 'Nothing
  HasTotalFieldP field V1
    = 'Nothing

type family HasTotalTypeP (typ :: Type) f :: Maybe Type where
  HasTotalTypeP typ (S1 _ (K1 _ typ))
    = 'Just typ
  HasTotalTypeP typ (l :*: r)
    = Alt (HasTotalTypeP typ l) (HasTotalTypeP typ r)
  HasTotalTypeP typ (l :+: r)
    = Both (HasTotalTypeP typ l) (HasTotalTypeP typ r)
  HasTotalTypeP typ (S1 _ _)
    = 'Nothing
  HasTotalTypeP typ (C1 _ f)
    = HasTotalTypeP typ f
  HasTotalTypeP typ (D1 _ f)
    = HasTotalTypeP typ f
  HasTotalTypeP typ (K1 _ _)
    = 'Nothing
  HasTotalTypeP typ U1
    = 'Nothing
  HasTotalTypeP typ V1
    = 'Nothing

data Pos (p :: Nat)

type family HasTotalPositionP (pos :: Nat) f :: Maybe Type where
  HasTotalPositionP pos (S1 _ (K1 (Pos pos) t))
    = 'Just t
  HasTotalPositionP pos (l :*: r)
    = Alt (HasTotalPositionP pos l) (HasTotalPositionP pos r)
  HasTotalPositionP pos (l :+: r)
    = Both (HasTotalPositionP pos l) (HasTotalPositionP pos r)
  HasTotalPositionP pos (S1 _ _)
    = 'Nothing
  HasTotalPositionP pos (C1 _ f)
    = HasTotalPositionP pos f
  HasTotalPositionP pos (D1 _ f)
    = HasTotalPositionP pos f
  HasTotalPositionP pos (K1 _ _)
    = 'Nothing
  HasTotalPositionP pos U1
    = 'Nothing
  HasTotalPositionP pos V1
    = 'Nothing

type family HasPartialTypeP a f :: Bool where
  HasPartialTypeP t (l :+: r)
    = HasPartialTypeP t l || HasPartialTypeP t r
  HasPartialTypeP t (C1 m f)
    = t == GTypes f
  HasPartialTypeP t (M1 _ _ f)
    = HasPartialTypeP t f
  HasPartialTypeP t _
    = 'False

type family HasCtorP (ctor :: Symbol) f :: Bool where
  HasCtorP ctor (C1 ('MetaCons ctor _ _) _)
    = 'True
  HasCtorP ctor (f :+: g)
    = HasCtorP ctor f || HasCtorP ctor g
  HasCtorP ctor (D1 m f)
    = HasCtorP ctor f
  HasCtorP ctor _
    = 'False

type family GTypes (rep :: Type -> Type) :: [Type] where
  GTypes (l :*: r)
    = GTypes l ++ GTypes r
  GTypes (K1 _ a)
    = '[ a]
  GTypes (M1 _ m a)
    = GTypes a
  GTypes U1 = '[]

