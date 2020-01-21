{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Constraints
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Constrained traversals.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Constraints
  ( -- *Traversals
    --
    --  $example
    HasConstraints (..)
  , HasConstraints' (..)
  ) where

import Data.Generics.Product.Internal.Constraints
import Data.Kind (Constraint)

import GHC.Generics (Generic (Rep), from, to)
import Data.Generics.Internal.VL.Traversal

class HasConstraints' (c :: * -> Constraint) s where
  constraints' :: TraversalC' c s

instance
  ( Generic s
  , GHasConstraints' c (Rep s)
  ) => HasConstraints' c s where
  constraints' = confusingC @c (\f s -> to <$> gconstraints' @c f (from s))
  {-# INLINE constraints' #-}

class HasConstraints (c :: * -> * -> Constraint) s t where
  constraints :: TraversalC c s t

instance
  ( Generic s
  , Generic t
  , GHasConstraints c (Rep s) (Rep t)
  ) => HasConstraints c s t where
  constraints f s = to <$> gconstraints @c f (from s)
  {-# INLINE constraints #-}
