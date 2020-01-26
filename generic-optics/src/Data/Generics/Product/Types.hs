{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
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
-- Copyright   :  (C) 2020 Csongor Kiss
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
    -- $setup
    HasTypes
  , types

    -- * Custom traversal strategies
    -- $custom
  , Children
  , ChGeneric

  , HasTypesUsing
  , typesUsing

  , HasTypesCustom (typesCustom)

  , GHasTypes (..)
  , typesUsing_
  ) where

import Optics.Core hiding (to)

import "generic-lens-core" Data.Generics.Internal.Errors
import "generic-lens-core" Data.Generics.Product.Internal.Types

import Data.Kind
import GHC.Generics
import GHC.TypeLits

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDeriveGeneric
-- >>> :set -XScopedTypeVariables
-- >>> import GHC.Generics
-- >>> import Optics.Core
-- >>> :{
-- data WTree a w
--   = Leaf a
--   | Fork (WTree a w) (WTree a w)
--   | WithWeight (WTree a w) w
--   deriving (Generic, Show)
-- :}

--------------------------------------------------------------------------------
-- HasTypes
--------------------------------------------------------------------------------

-- | Traverse all types in the given structure.
--
-- For example, to update all 'String's in a @WTree (Maybe String) String@, we can write
-- 
-- >>> myTree = WithWeight (Fork (Leaf (Just "hello")) (Leaf Nothing)) "world"
-- >>> over (types @String) (++ "!") myTree
-- WithWeight (Fork (Leaf (Just "hello!")) (Leaf Nothing)) "world!"
--
-- The traversal is /deep/, which means that not just the immediate
-- children are visited, but all nested values too.
types :: forall a s. HasTypes s a => Traversal' s a
types = types_ @s @a
{-# INLINE types #-}

pure' :: Traversal s s a b
pure' = traversalVL (const pure)
{-# INLINE pure' #-}

class HasTypes s a where
  types_ :: Traversal' s a

  types_ = pure'
  {-# INLINE types_ #-}

instance
  ( HasTypesUsing ChGeneric s s a a
  ) => HasTypes s a where
  types_ = typesUsing_ @ChGeneric
  {-# INLINE types_ #-}

--------------------------------------------------------------------------------
data Void
instance {-# OVERLAPPING #-} HasTypes Void a where
  types_ = pure'

instance {-# OVERLAPPING #-} HasTypes s Void where
  types_ = pure'

instance {-# OVERLAPPING #-} HasTypesUsing ch Void Void a b where
  typesUsing_ = pure'

instance {-# OVERLAPPING #-} HasTypesUsing ch s s Void Void where
  typesUsing_ = pure'
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- HasTypesUsing
--------------------------------------------------------------------------------

-- $custom
--
-- The default traversal strategy 'types' recurses into each node of the type
-- using the 'Generic' instance for the nodes. However, in general not all
-- nodes will have a 'Generic' instance. For example:
--
-- >>> data Opaque = Opaque String deriving Show
-- >>> myTree = WithWeight (Fork (Leaf (Opaque "foo")) (Leaf (Opaque "bar"))) False
-- >>> over (types @String) (++ "!") myTree
-- ...
-- ... | No instance for ‘Generic Opaque’
-- ... |   arising from a generic traversal.
-- ... |   Either derive the instance, or define a custom traversal using HasTypesCustom
-- ...
--
-- In these cases, we can define a custom traversal strategy to override the
-- generic behaviour for certain types.
-- For a self-contained example, see the CustomChildren module in the tests directory.

-- | @since 1.2.0.0
typesUsing :: forall ch a s. HasTypesUsing ch s s a a => Traversal' s a
typesUsing = typesUsing_ @ch @s @s @a
{-# INLINE typesUsing #-}

-- | @since 1.2.0.0
class HasTypesUsing (ch :: Type) s t a b where
  typesUsing_ :: Traversal s t a b

instance {-# OVERLAPPABLE #-}
  ( HasTypesOpt ch (Interesting ch a s) s t a b
  ) => HasTypesUsing ch s t a b where
  typesUsing_ = typesOpt @ch @(Interesting ch a s)
  {-# INLINE typesUsing_ #-}

instance {-# OVERLAPPABLE #-} HasTypesUsing ch a b a b where
  typesUsing_ = traversalVL id

-- | By adding instances to this class, we can override the default
-- behaviour in an ad-hoc manner.
-- For example:
--
-- @
-- instance HasTypesCustom Custom Opaque Opaque String String where
--   typesCustom f (Opaque str) = Opaque <$> f str
-- @
--
-- @since 1.2.0.0
class HasTypesCustom (ch :: Type) s t a b where
  -- | This function should never be used directly, only to override
  -- the default traversal behaviour. To actually use the custom
  -- traversal strategy, see 'typesUsing'. This is because 'typesUsing' does
  -- additional optimisations, like ensuring that nodes with no relevant members will
  -- not be traversed at runtime.
  typesCustom :: Traversal s t a b

instance {-# OVERLAPPABLE #-}
  ( GHasTypes ch (Rep s) (Rep t) a b
  , Generic s
  , Generic t
  -- if there's no Generic instance here, it means we got through the
  -- Children check by a user-defined custom strategy.
  -- Therefore, we can ignore the missing Generic instance, and
  -- instead report a missing HasTypesCustom instance
  , Defined (Rep s)
    (PrettyError '[ 'Text "No instance " ':<>: QuoteType (HasTypesCustom ch s t a b)])
    (() :: Constraint)
  ) => HasTypesCustom ch s t a b where
  typesCustom = traversalVL (\f s -> to <$> traverseOf (gtypes_ @ch )f (from s))

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------
-- TODO: these should never leak out in error messages

class HasTypesOpt (ch :: Type) (p :: Bool) s t a b where
  typesOpt :: Traversal s t a b

instance HasTypesCustom ch s t a b => HasTypesOpt ch 'True s t a b where
  typesOpt = typesCustom @ch

instance HasTypesOpt ch 'False s s a b where
  typesOpt = pure'

--------------------------------------------------------------------------------
-- TODO: pull out recursion here.

class GHasTypes ch s t a b where
  gtypes_ :: Traversal (s x) (t x) a b

instance
  ( GHasTypes ch l l' a b
  , GHasTypes ch r r' a b
  ) => GHasTypes ch (l :*: r) (l' :*: r') a b where
  gtypes_ = traversalVL (\f (l :*: r) -> (:*:) <$> traverseOf (gtypes_ @ch) f l <*> traverseOf (gtypes_ @ch) f r)
  {-# INLINE gtypes_ #-}

instance
  ( GHasTypes ch l l' a b
  , GHasTypes ch r r' a b
  ) => GHasTypes ch (l :+: r) (l' :+: r') a b where
  gtypes_ = traversalVL (\f -> \case
                            L1 l -> L1 <$> traverseOf (gtypes_ @ch) f l
                            R1 r -> R1 <$> traverseOf (gtypes_ @ch) f r)
  {-# INLINE gtypes_ #-}

instance GHasTypes ch s t a b => GHasTypes ch (M1 m meta s) (M1 m meta t) a b where
  gtypes_ = traversalVL (\f (M1 s) -> M1 <$> traverseOf (gtypes_ @ch) f s)
  {-# INLINE gtypes_ #-}

-- In the recursive case, we invoke 'HasTypesUsing' again, using the
-- same strategy
-- This instance is marked INCOHERENT, because
instance {-# INCOHERENT #-} HasTypesUsing ch s t a b => GHasTypes ch (Rec0 s) (Rec0 t) a b where
  gtypes_ = traversalVL (\f (K1 x) -> K1 <$> traverseOf (typesUsing_ @ch) f x)
  {-# INLINE gtypes_ #-}

-- | The default instance for 'HasTypes' acts as a synonym for
-- 'HasTypesUsing ChGeneric', so in most cases this instance should
-- behave the same as the one above.
-- However, there might be overlapping instances defined for
-- 'HasTypes' directly, in which case we want to prefer those
-- instances (even though the custom instances should always be added to 'HasTypesCustom')
instance {-# OVERLAPPING #-} HasTypes b a => GHasTypes ChGeneric (Rec0 b) (Rec0 b) a a where
  gtypes_ = traversalVL (\f (K1 x) -> K1 <$> traverseOf (types_ @b @a) f x)
  {-# INLINE gtypes_ #-}

instance GHasTypes ch U1 U1 a b where
  gtypes_ = pure'
  {-# INLINE gtypes_ #-}

instance GHasTypes ch V1 V1 a b where
  gtypes_ = pure'
  {-# INLINE gtypes_ #-}

