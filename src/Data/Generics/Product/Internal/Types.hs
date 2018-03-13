{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.Types
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Types
  (
    GHasConstraints (..)
  , HList (..)
  ) where

import Data.Kind (Type, Constraint)
import GHC.Generics
import Data.Generics.Internal.VL.Iso

-- | Constrained traversal.
class GHasConstraints (c :: * -> * -> Constraint) (s :: * -> *) (t :: * -> *) where
  gconstraints :: forall f x.
    Applicative f => (forall a b. c a b => a -> f b) -> s x -> f (t x)

instance (GHasConstraints c l l', GHasConstraints c r r') => GHasConstraints c (l :*: r) (l' :*: r') where
  gconstraints f (l :*: r) = (:*:) <$> gconstraints @c f l <*> gconstraints @c f r
  {-# INLINE gconstraints #-}

instance (GHasConstraints c l l', GHasConstraints c r r') => GHasConstraints c (l :+: r) (l' :+: r') where
  gconstraints f (L1 l) = L1 <$> gconstraints @c f l
  gconstraints f (R1 r) = R1 <$> gconstraints @c f r
  {-# INLINE gconstraints #-}

instance c a b => GHasConstraints c (K1 R a) (K1 R b) where
  gconstraints = kIso
  {-# INLINE gconstraints #-}

instance GHasConstraints c s t => GHasConstraints c (M1 i m s) (M1 i m t) where
  gconstraints f (M1 x) = M1 <$> gconstraints @c f x
  {-# INLINE gconstraints #-}

instance GHasConstraints c U1 U1 where
  gconstraints _ _ = pure U1
  {-# INLINE gconstraints #-}

--------------------------------------------------------------------------------

-- | Multi-traversal
--type GHasTypes rep as = GHasConstraints (Contains as) rep

-- Try to use a function from the list, or default to 'pure' if not present
{-
gtypes
  :: forall as x f g.
  ( GHasTypes f as
  , Applicative g
  ) => HList (Functions as g) -> f x -> g (f x)
gtypes hl s = gconstraints @(Contains as) (pick hl) s
-}

--------------------------------------------------------------------------------

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:>) :: a -> HList as -> HList (a ': as)
infixr 5 :>

-- >>> :kind! Functions '[Int, Char, Bool] Maybe
-- '[Int -> Maybe Int, Char -> Maybe Char, Bool -> Maybe Bool]
type family Functions (ts :: [Type]) (g :: Type -> Type) = r | r -> ts where
  Functions '[] _ = '[]
  Functions (t ': ts) g = ((t -> g t) ': Functions ts g)

class Contains as a where
  pick :: Applicative g => HList (Functions as g) -> a -> g a

instance {-# OVERLAPPING #-} Contains (a ': as) a where
  pick (h :> _) = h

instance Contains as a => Contains (b ': as) a where
  pick (_ :> hs) = pick hs

instance Contains '[] a where
  pick _ = pure
