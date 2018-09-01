{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.Subtype
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structural subtype relationships between product types.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Subtype
  ( GUpcast (..)
  , GSmash (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Product.Internal.GLens

import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (Symbol)
import Data.Generics.Internal.Profunctor.Lens (view)

--------------------------------------------------------------------------------
-- * Generic upcasting

class GUpcast (sub :: Type -> Type) (sup :: Type -> Type) where
  gupcast :: sub p -> sup p

instance (GUpcast sub a, GUpcast sub b) => GUpcast sub (a :*: b) where
  gupcast rep = gupcast rep :*: gupcast rep

instance
  GLens' (HasTotalFieldPSym field) sub t
  => GUpcast sub (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) where

  gupcast r = M1 (K1 (view (glens @(HasTotalFieldPSym field)) r))

instance GUpcast sub sup => GUpcast sub (C1 c sup) where
  gupcast = M1 . gupcast

instance GUpcast sub sup => GUpcast sub (D1 c sup) where
  gupcast = M1 . gupcast

--------------------------------------------------------------------------------
-- * Generic smashing

class GSmash sub sup where
  gsmash :: sup p -> sub p -> sub p

instance (GSmash a sup, GSmash b sup) => GSmash (a :*: b) sup where
  gsmash rep (a :*: b) = gsmash rep a :*: gsmash rep b

instance
  ( leaf ~ (S1 ('MetaSel ('Just field) p f b) t)
  , GSmashLeaf leaf sup (HasTotalFieldP field sup)
  ) => GSmash (S1 ('MetaSel ('Just field) p f b) t) sup where

  gsmash = gsmashLeaf @_ @_ @(HasTotalFieldP field sup)

instance GSmash sub sup => GSmash (C1 c sub) sup where
  gsmash sup (M1 sub) = M1 (gsmash sup sub)

instance GSmash sub sup => GSmash (D1 c sub) sup where
  gsmash sup (M1 sub) = M1 (gsmash sup sub)

class GSmashLeaf sub sup (w :: Maybe Type) where
  gsmashLeaf :: sup p -> sub p -> sub p

instance
  GLens' (HasTotalFieldPSym field) sup t
  => GSmashLeaf (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) sup ('Just t) where
  gsmashLeaf sup _ = M1 (K1 (view (glens @(HasTotalFieldPSym field)) sup))

instance GSmashLeaf (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) sup 'Nothing where
  gsmashLeaf _ = id

data HasTotalFieldPSym :: Symbol -> (TyFun (Type -> Type) (Maybe Type))
type instance Eval (HasTotalFieldPSym sym) tt = HasTotalFieldP sym tt
