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

import Data.Type.Bool     (type (||), type (&&))
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits (Symbol, Nat)
import Data.Kind (Type)

import Data.Generics.Product.Internal.HList

-- Note: these could be factored out into a single traversal

type family HasTotalFieldP (field :: Symbol) f :: Bool where
  HasTotalFieldP field (S1 ('MetaSel ('Just field) _ _ _) _)
    = 'True
  HasTotalFieldP field (l :*: r)
    = HasTotalFieldP field l || HasTotalFieldP field r
  HasTotalFieldP field (l :+: r)
    = HasTotalFieldP field l && HasTotalFieldP field r
  HasTotalFieldP field (S1 _ _)
    = 'False
  HasTotalFieldP field (C1 _ f)
    = HasTotalFieldP field f
  HasTotalFieldP field (D1 _ f)
    = HasTotalFieldP field f
  HasTotalFieldP field (K1 _ _)
    = 'False
  HasTotalFieldP field U1
    = 'False
  HasTotalFieldP field V1
    = 'False

type family HasTotalTypeP (typ :: Type) f :: Bool where
  HasTotalTypeP typ (S1 _ (K1 _ typ))
    = 'True
  HasTotalTypeP typ (l :*: r)
    = HasTotalTypeP typ l || HasTotalTypeP typ r
  HasTotalTypeP typ (l :+: r)
    = HasTotalTypeP typ l && HasTotalTypeP typ r
  HasTotalTypeP typ (S1 _ _)
    = 'False
  HasTotalTypeP typ (C1 _ f)
    = HasTotalTypeP typ f
  HasTotalTypeP typ (D1 _ f)
    = HasTotalTypeP typ f
  HasTotalTypeP typ (K1 _ _)
    = 'False
  HasTotalTypeP typ U1
    = 'False
  HasTotalTypeP typ V1
    = 'False

data Pos (p :: Nat)

type family HasTotalPositionP (pos :: Nat) f :: Bool where
  HasTotalPositionP pos (S1 _ (K1 (Pos pos) _))
    = 'True
  HasTotalPositionP pos (l :*: r)
    = HasTotalPositionP pos l || HasTotalPositionP pos r
  HasTotalPositionP pos (l :+: r)
    = HasTotalPositionP pos l && HasTotalPositionP pos r
  HasTotalPositionP pos (S1 _ _)
    = 'False
  HasTotalPositionP pos (C1 _ f)
    = HasTotalPositionP pos f
  HasTotalPositionP pos (D1 _ f)
    = HasTotalPositionP pos f
  HasTotalPositionP pos (K1 _ _)
    = 'False
  HasTotalPositionP pos U1
    = 'False
  HasTotalPositionP pos V1
    = 'False

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

type family GTypes (rep :: Type -> Type) :: [(Type)] where
  GTypes (l :*: r)
    = GTypes l ++ GTypes r
  GTypes (K1 _ a)
    = '[ a]
  GTypes (M1 _ m a)
    = GTypes a
  GTypes U1 = '[]

