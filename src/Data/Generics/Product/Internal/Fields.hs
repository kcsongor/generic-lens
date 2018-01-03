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

import Data.Generics.Internal.Lens
import Data.Generics.Product.Internal.List

import Data.Kind    (Type)
import GHC.Generics
import GHC.TypeLits (Symbol, Nat, type (+))

-- |As 'HasField' but over generic representations as defined by
--  "GHC.Generics".
class GHasField (field :: Symbol) (s :: Type -> Type) (t :: Type -> Type) a b | s field -> a, t field -> b where
  gfield :: Lens (s x) (t x) a b

type GHasField' field s a = GHasField field s s a a

instance (GHasField field l l' a b, GHasField field r r' a b) =>  GHasField field (l :+: r) (l' :+: r') a b where

  gfield = sumIso . choosing (gfield @field) (gfield @field)

instance (GHasField field f g a b) => GHasField field (M1 D meta f) (M1 D meta g) a b where
  gfield = mLens . gfield @field

class Elem (as :: [(Symbol, *)]) (field :: Symbol) (i :: Nat) a | as field -> i a
instance pos ~ 0 => Elem ('(field, a) ': xs) field pos a
instance {-# OVERLAPPABLE #-} (Elem xs field i a, pos ~ (i + 1)) => Elem (x ': xs) field pos a

instance (Elem as field i a, Elem bs field i b,  IndexList i as bs a b, GIsList f g as bs) => GHasField field (M1 C meta f) (M1 C meta g) a b where
  gfield = mLens . glist . point @i
