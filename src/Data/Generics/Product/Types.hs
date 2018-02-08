{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
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
  ) where

import Data.Generics.Product.Internal.Types
import Data.Kind (Constraint)

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

instance
  ( HasConstraints' ((~) a) s
  ) => HasTypesDeep a s where

  typesDeep f s = constraints' @((~) a) f s

--------------------------------------------------------------------------------
type family Primitive a :: Bool where
  Primitive Int  = 'True
  Primitive Char = 'True
  Primitive Bool = 'True
  Primitive _    = 'False
  -- TODO: more primitives, or perhaps abstract from stop condition

-- TODO: refactor
-- deep constrained traversals, stopping at primitive types
class HasConstraints' (c :: * -> Constraint) s where
  constraints' :: forall g.
    Applicative g => (forall a. c a => a -> g a) -> s -> g s

instance (Generic s, GHasConstraints' c (Rep s)) => HasConstraints' c s where
  constraints' f s = to <$> gconstraints' @c f (from s)

class GHasConstraints' (c :: * -> Constraint) (f :: * -> *) where
  gconstraints' :: forall g x.
    Applicative g => (forall a. c a => a -> g a) -> f x -> g (f x)

instance (GHasConstraints' c l, GHasConstraints' c r) => GHasConstraints' c (l :*: r) where
  gconstraints' f (l :*: r) = (:*:) <$> gconstraints' @c f l <*> gconstraints' @c f r

instance (GHasConstraints' c l, GHasConstraints' c r) => GHasConstraints' c (l :+: r) where
  gconstraints' f (L1 l) = L1 <$> gconstraints' @c f l
  gconstraints' f (R1 r) = R1 <$> gconstraints' @c f r

instance GRec c (Primitive a) (K1 R a) => GHasConstraints' c (K1 R a) where
  gconstraints' = grec @c @(Primitive a)

instance GHasConstraints' c f => GHasConstraints' c (M1 m meta f) where
  gconstraints' f (M1 x) = M1 <$> gconstraints' @c f x

instance GHasConstraints' c U1 where
  gconstraints' _ _ = pure U1

class GRec (c :: * -> Constraint) (s :: Bool) (f :: * -> *) where
  grec :: forall g x.
    Applicative g => (forall a. c a => a -> g a) -> f x -> g (f x)

instance HasConstraints' c a => GRec c 'False (K1 R a) where
  grec f (K1 s) = K1 <$> constraints' @c @a f s

instance c a => GRec c 'True (K1 R a) where
  grec f s = kIso f s
