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
    Core.HasTypes
  , types

    -- * Custom traversal strategies
    -- $custom
  , Core.Children
  , Core.ChGeneric

  , Core.HasTypesUsing
  , typesUsing

  , Core.HasTypesCustom (typesCustom)

  , Core.GHasTypes (..)
  , Core.typesUsing_
  ) where

import qualified "generic-lens-core" Data.Generics.Internal.VL.Traversal as VL
import qualified "generic-lens-core" Data.Generics.Product.Internal.Types as Core

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDeriveGeneric
-- >>> :set -XScopedTypeVariables
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.VL.Traversal
-- >>> :m +Data.Generics.Internal.VL.Lens
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
types :: forall a s. Core.HasTypes s a => VL.Traversal' s a
types = VL.confusing (Core.types_ @s @a)
{-# INLINE types #-}

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
typesUsing :: forall ch a s. Core.HasTypesUsing ch s s a a => VL.Traversal' s a
typesUsing = VL.confusing (Core.typesUsing_ @ch @s @s @a)
{-# INLINE typesUsing #-}
