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

import Data.Generics.Product.Internal.List
import Data.Kind      (Type)
import Data.Type.Bool (If, Not)
import GHC.Generics
import GHC.TypeLits   (type (<=?), type (+), type (-), Nat)
import Data.Generics.Internal.Profunctor.Lens
import Data.Generics.Internal.Profunctor.Iso

-- |As 'HasPosition' but over generic representations as defined by
--  "GHC.Generics".
class GHasPosition (i :: Nat) (s :: Type -> Type) (t :: Type -> Type) a b | s i -> a, t i -> b, s t i -> b where
  gposition :: Lens (s x) (t x) a b

type GHasPosition' i s a = GHasPosition i s s a a

instance (GHasPosition i l l' a b, GHasPosition i r r' a b) =>  GHasPosition i (l :+: r) (l' :+: r') a b where
  gposition = sumIso  . choosing (gposition @i) (gposition @i)
  {-# INLINE gposition #-}

instance GHasPosition i s t a b => GHasPosition i (M1 D meta s) (M1 D meta t) a b where
  gposition = mLens . gposition @i
  {-# INLINE gposition #-}

instance
  ( IndexList (i - 1) as bs a b
  , GIsList () f g as bs
  ) => GHasPosition i (M1 C meta f) (M1 C meta g) a b where
  gposition = mIso . glist @() . point @(i - 1)
  {-# INLINE gposition #-}

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
