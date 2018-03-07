{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Types
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Types
  ( -- *Traversals
    --
    --  $example
    HasTypes (..)
  , HasTypesDeep (..)
  , Node (..)
  , Primitive (..)
  , Recurse (..)
  , Found (..)
  , FoundAndRecurse (..)
  , SStop (..)
  ) where

import Data.Generics.Product.Internal.Types
import Data.Generics.Internal.GenericDeep
import Data.Type.Bool
import Data.Kind
import Data.Coerce

import GHC.Generics
import Data.Generics.Internal.VL.Traversal
import Data.Generics.Internal.VL.Iso

class HasTypes a s where
  types :: Traversal' s a

instance
  ( Generic s
  , GHasTypes (Rep s) '[a]
  ) => HasTypes a s where
  types = confusing (\f -> (repIso . gtypes) (f :> HNil))
  {-# INLINE types #-}

class HasTypesDeep a s where
  typesDeep :: Traversal' s a

  default typesDeep :: Traversal' s a
  typesDeep _ = pure

instance
  ( GHasTypesDeep a route
  , GenericDeep s
  , Node tutu route ~ BuildRoute (FindInteresting (DeepN s) a) (DeepN s) a
  , Coercible (Deep s) route
  ) => HasTypesDeep a s where
  typesDeep f s = toDeep . tr <$> gtypesDeep @_ @route f (fr $ fromDeep s)
    where fr :: forall x. Deep s x -> route x
          fr = coerce
          tr :: forall x. route x -> Deep s x
          tr = coerce
  {-# INLINE[2] typesDeep #-}

instance {-# OVERLAPPING #-} HasTypesDeep a Bool
instance {-# OVERLAPPING #-} HasTypesDeep a Char
instance {-# OVERLAPPING #-} HasTypesDeep a Double
instance {-# OVERLAPPING #-} HasTypesDeep a Float
instance {-# OVERLAPPING #-} HasTypesDeep a Int
instance {-# OVERLAPPING #-} HasTypesDeep a Integer
instance {-# OVERLAPPING #-} HasTypesDeep a Ordering

--------------------------------------------------------------------------------

class GHasTypesDeep a s where
  gtypesDeep :: Traversal' (s x) a

instance GHasTypesDeep a (SStop b) where
  gtypesDeep _ = pure
  {-# INLINE[0] gtypesDeep #-}

instance GHasTypesDeep a (Found a) where
  gtypesDeep f (Found (K1 a)) = Found . K1 <$> f a
  {-# INLINE[0] gtypesDeep #-}

instance GHasTypesDeep a U1 where
  gtypesDeep _ = pure
  {-# INLINE[0] gtypesDeep #-}

instance
  ( HasTypesDeep a t
  ) => GHasTypesDeep a (Recurse t) where
  gtypesDeep f (Recurse (K1 a)) = Recurse . K1 <$> typesDeep f a
  {-# INLINE[0] gtypesDeep #-}

instance GHasTypesDeep a (Rec0 a) where
  gtypesDeep f (K1 x) = K1 <$> f x
  {-# INLINE[0] gtypesDeep #-}

instance
  ( GHasTypesDeep a route
  , GenericDeep t
  , Coercible (Deep t) route
  ) => GHasTypesDeep a (Node t route) where
  gtypesDeep f (Node (K1 r))
    = Node . K1 . toDeep . tr <$> gtypesDeep @_ @route f (fr $ fromDeep r)
    where fr :: forall x. Deep t x -> route x
          fr = coerce
          tr :: forall x. route x -> Deep t x
          tr = coerce
  {-# INLINE[2] gtypesDeep #-}

instance
  ( GHasTypesDeep a l
  , GHasTypesDeep a r
  ) => GHasTypesDeep a (l :*: r) where
  gtypesDeep f (l :*: r) = (:*:) <$> gtypesDeep f l <*> gtypesDeep f r
  {-# INLINE[0] gtypesDeep #-}

instance
  ( GHasTypesDeep a l
  , GHasTypesDeep a r
  ) => GHasTypesDeep a (l :+: r) where
  gtypesDeep f (L1 l) = L1 <$> gtypesDeep f l
  gtypesDeep f (R1 r) = R1 <$> gtypesDeep f r
  {-# INLINE[0] gtypesDeep #-}

---------

type family BuildRoute (colours :: Set) s a :: * -> * where
  BuildRoute map (Node typ tree) a
    = If (IsInteresting typ map) (Node typ (BuildRoute map tree a)) (SStop typ)
  BuildRoute map (l :*: r) a
    = (BuildRoute map l a) :*: (BuildRoute map r a)
  BuildRoute map (l :+: r) a
    = (BuildRoute map l a) :+: (BuildRoute map r a)
  BuildRoute map (Rec0 a) a
    = FoundAndRecurse a
  BuildRoute map (Rec0 a) _
    = Recurse a
  BuildRoute map (Primitive a) a
    = Found a
  BuildRoute map (Primitive a) _
    = SStop a
  BuildRoute map U1 _
    = U1

type DeepN t = Node t (Deep t)

--------------------------------------------------------------------------------
data Set = Set [Type]

type family UnSet (m :: Set) :: [Type] where
  UnSet ('Set m) = m

type EmptySet = 'Set '[]

type family Mark (m :: Set) (k :: Type) :: Set where
  Mark ('Set '[]) k = 'Set '[ k ]
  Mark ('Set (k ': ms)) k = 'Set (k ': ms)
  Mark ('Set (m ': ms)) k = 'Set (m ': UnSet (Mark ('Set ms) k))

type family MarkAll (ts :: [*]) (m :: Set) :: Set where
  MarkAll '[] m = m
  MarkAll (t ': ts) m = MarkAll ts (Mark m t)

type family IsInteresting (t :: Type) (m :: Set) :: Bool where
  IsInteresting _ ('Set '[]) = 'False
  IsInteresting t ('Set (t  ': _)) = 'True
  IsInteresting t ('Set (_ ': m)) = IsInteresting t ('Set m)

--------------------------------------------------------------------------------

type family FindInteresting' (haystack :: * -> *) (needle :: *) (path :: [*]) (m :: Set) :: Set where
  FindInteresting' (Node a tree) a p map
    = FindInteresting' tree a (a ': p) (MarkAll p map)
  FindInteresting' (Node typ tree) a p map
    = FindInteresting' tree a (typ ': p) map
  FindInteresting' (l :*: r) a p map
    = (FindInteresting' l a p (FindInteresting' r a p map))
  FindInteresting' (l :+: r) a p map
    = (FindInteresting' r a p (FindInteresting' l a p map))
  FindInteresting' (Primitive typ) typ p map
    = MarkAll p map
  FindInteresting' (Primitive typ) a p map
    = map
  FindInteresting' (Rec0 typ) typ p map
    = MarkAll p map
  FindInteresting' (Rec0 typ) a p map
    = If (IsInteresting typ map) (MarkAll p map) map
  FindInteresting' U1 a p map
    = map

type FindInteresting structure typ = FindInteresting' structure typ '[] EmptySet
