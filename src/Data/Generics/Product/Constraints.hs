{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
  , Deep
  ) where

import Data.Generics.Product.Internal.Types
import Data.Kind (Constraint)

import GHC.Generics (Generic (Rep), from, to)
import Data.Generics.Internal.VL.Traversal

class HasConstraints (c :: * -> Constraint) s where
  constraints :: Applicative g => (forall a. c a => a -> g a) -> s -> g s

  default constraints :: Applicative g => (forall a. c a => a -> g a) -> s -> g s
  constraints _ = pure

instance
  ( Generic s
  , GHasConstraints c (Rep s)
  ) => HasConstraints c s where
  constraints = confusingC @c (\f s -> to <$> gconstraints @c f (from s))
  {-# INLINE constraints #-}

instance {-# OVERLAPPING #-} HasConstraints c Bool
instance {-# OVERLAPPING #-} HasConstraints c Char
instance {-# OVERLAPPING #-} HasConstraints c Double
instance {-# OVERLAPPING #-} HasConstraints c Float
instance {-# OVERLAPPING #-} HasConstraints c Int
instance {-# OVERLAPPING #-} HasConstraints c Integer
instance {-# OVERLAPPING #-} HasConstraints c Ordering

class (c a, HasConstraints (Deep c) a) => Deep c a
instance (c a, HasConstraints (Deep c) a) => Deep c a
