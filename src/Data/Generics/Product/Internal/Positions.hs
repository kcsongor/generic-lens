{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.Positions
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive positional product type getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Positions
  ( GHasPosition (..)
  , GHasPosition'
  , type (<?)
  , Size
  ) where

import Data.Generics.Internal.Lens

import Data.Kind      (Type)
import Data.Type.Bool (If, Not)
import GHC.Generics
import GHC.TypeLits   (type (<=?), type (+), Nat)

-- |As 'HasPosition' but over generic representations as defined by
--  "GHC.Generics".
class GHasPosition (offset :: Nat) (i :: Nat) (s :: Type -> Type) (t :: Type -> Type) a b | s offset i -> a, s t offset i -> b where
  gposition :: Lens (s x) (t x) a b

type GHasPosition' i s a = GHasPosition 1 i s s a a

instance GHasPosition i i (K1 R a) (K1 R b) a b where
  gposition f (K1 x) = fmap K1 (f x)

instance GHasPosition offset i s t a b => GHasPosition offset i (M1 m meta s) (M1 m meta t) a b where
  gposition = mLens . gposition @offset @i

instance
  ( goLeft  ~ (i <? (offset + Size l))
  , offset' ~ (If goLeft offset (offset + Size l))
  , GProductHasPosition offset' i l r l' r' a b goLeft
  ) => GHasPosition offset i (l :*: r) (l' :*: r') a b where

  gposition = gproductPosition @offset' @i @_ @_ @_ @_ @_ @_ @goLeft

class GProductHasPosition (offset :: Nat) (i :: Nat) l r l' r' a b (left :: Bool) | offset i l r -> a, offset i l r l' r' -> b where
  gproductPosition :: Lens ((l :*: r) x) ((l' :*: r') x) a b

instance GHasPosition offset i l l' a b => GProductHasPosition offset i l r l' r a b 'True where
  gproductPosition = first . gposition @offset @i

instance GHasPosition offset i r r' a b => GProductHasPosition offset i l r l r' a b 'False where
  gproductPosition = second . gposition @offset @i

type family Size f :: Nat where
  Size (l :*: r)
    = Size l + Size r
  Size (l :+: r)
    = Min (Size l) (Size r)
  Size (D1 meta f)
    = Size f
  Size (C1 meta f)
    = Size f
  Size f
    = 1

--------------------------------------------------------------------------------

type x <? y = Not (y <=? x)
infixl 4 <?

type Min a b = If (a <? b) a b
