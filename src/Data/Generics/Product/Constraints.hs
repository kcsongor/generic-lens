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
-- Copyright   :  (C) 2018 Csongor Kiss
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
  ) where

import Data.Generics.Product.Internal.Types
import Data.Kind (Constraint)

import GHC.Generics (Generic (Rep), from, to)
import Data.Generics.Internal.VL.Traversal

--class HasConstraints (c :: * -> Constraint) s where
--  constraints :: Applicative g => (forall a. c a => a -> g a) -> s -> g s
--
--instance
--  ( Generic s
--  , GHasConstraints c (Rep s)
--  ) => HasConstraints c s where
--  constraints = confusingC @c (\f s -> to <$> gconstraints @c f (from s))
--  {-# INLINE constraints #-}

class HasConstraints (c :: * -> * -> Constraint) s t where
  constraints :: Applicative g => (forall a b. c a b => a -> g b) -> s -> g t

instance
  ( Generic s
  , Generic t
  , GHasConstraints c (Rep s) (Rep t)
  ) => HasConstraints c s t where
  constraints f s = to <$> gconstraints @c f (from s)
  {-# INLINE constraints #-}

