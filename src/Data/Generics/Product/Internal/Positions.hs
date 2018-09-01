{-# LANGUAGE TypeInType #-}
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
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive positional product type getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Positions
  ( type (<?)
  , Size
  , CRep
  ) where

import Data.Generics.Internal.Families.Has (Pos)
import Data.Kind      (Type)
import Data.Type.Bool (If, Not)
import GHC.Generics
import GHC.TypeLits   (type (<=?), type (+), Nat)

-- | Alias for the kind of the generic rep
type G = Type -> Type

--------------------------------------------------------------------------------

-- | In-order labeling of the generic tree with the field positions
--
-- We replace the (K1 R a) nodes with (K1 (Pos n) a), where 'n' is the position
-- of the field in question in the data type. This is convenient, because we
-- can reuse all the existing functions as long as they are polymorphic in the
-- first parameter of 'K1'.
type family CRep (a :: Type) :: G where
  CRep rep = Fst (Traverse (Rep rep) 1)

-- | The actual traversal.
--
-- Might be cleaner if the sum and product parts were separated (as there's
-- and invariant that 'n' should be zero when we're at a sum node, which holds
-- for derived Generic instances (where the sums are strictly above the products))
type family Traverse (a :: G) (n :: Nat) :: (G, Nat) where
  Traverse (M1 mt m s) n
    = Traverse1 (M1 mt m) (Traverse s n)
  Traverse (l :+: r) n
    = '(Fst (Traverse l n) :+: Fst (Traverse r n), n)
  Traverse (l :*: r) n
    = TraverseProd (:*:) (Traverse l n) r
  Traverse (K1 _ p) n
    = '(K1 (Pos n) p, n + 1)
  Traverse U1 n
    = '(U1, n)

type family Traverse1 (w :: G -> G) (z :: (G, Nat)) :: (G, Nat) where
  Traverse1 w '(i, n) = '(w i, n)

-- | For products, we first traverse the left-hand side, followed by the second
-- using the counter returned by the left traversal.
type family TraverseProd (c :: G -> G -> G) (a :: (G, Nat)) (r :: G) :: (G, Nat) where
  TraverseProd w '(i, n) r = Traverse1 (w i) (Traverse r n)

--------------------------------------------------------------------------------
-- Utilities

type family Fst (p :: (a, b)) :: a where
  Fst '(a, b) = a

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

type x <? y = Not (y <=? x)
infixl 4 <?

type Min a b = If (a <? b) a b
