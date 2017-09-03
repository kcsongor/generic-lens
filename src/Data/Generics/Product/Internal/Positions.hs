{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
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
class GHasPosition (offset :: Nat) (i :: Nat) (f :: Type -> Type) a | offset i f -> a where
  gposition :: Lens' (f x) a

instance GHasPosition i i (K1 R a) a where
  gposition f (K1 x) = fmap K1 (f x)

instance GHasPosition offset i f a => GHasPosition offset i (M1 m meta f) a where
  gposition = mIso . gposition @offset @i

instance
  ( goLeft  ~ (i <? (offset + Size l))
  , offset' ~ (If goLeft offset (offset + Size l))
  , GProductHasPosition offset' i l r a goLeft
  ) => GHasPosition offset i (l :*: r) a where

  gposition = gproductPosition @offset' @i @_ @_ @_ @goLeft


class GProductHasPosition (offset :: Nat) (i :: Nat) l r a (left :: Bool) | offset i l r left -> a where
  gproductPosition :: Lens' ((l :*: r) x) a

instance GHasPosition offset i l a => GProductHasPosition offset i l r a 'True where
  gproductPosition = first . gposition @offset @i

instance GHasPosition offset i r a => GProductHasPosition offset i l r a 'False where
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
