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
-- Module      :  Data.Generics.Product.Internal.Constraints
-- Copyright   :  (C) 2022 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constrained traversals
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Constraints
  (
    GHasConstraints (..)
  , GHasConstraints' (..)
  , TraversalC
  , TraversalC'
  , confusingC
  ) where

import Data.Kind (Type, Constraint)
import GHC.Generics
import Data.Generics.Internal.VL.Iso
import Data.Generics.Product.Internal.HList
import Data.Generics.Internal.VL

-- | Constrained traversal.
class GHasConstraints' (c :: * -> Constraint) (f :: * -> *) where
  gconstraints' :: forall g x.
    Applicative g => (forall a. c a => a -> g a) -> f x -> g (f x)

instance
  ( GHasConstraints' c l
  , GHasConstraints' c r
  ) => GHasConstraints' c (l :*: r) where
  gconstraints' f (l :*: r)
    = (:*:) <$> gconstraints' @c f l <*> gconstraints' @c f r

instance
  ( GHasConstraints' c l
  , GHasConstraints' c r
  ) => GHasConstraints' c (l :+: r) where
  gconstraints' f (L1 l) = L1 <$> gconstraints' @c f l
  gconstraints' f (R1 r) = R1 <$> gconstraints' @c f r

instance c a => GHasConstraints' c (Rec0 a) where
  gconstraints' f x = kIso f x

instance GHasConstraints' c f
  => GHasConstraints' c (M1 m meta f) where
  gconstraints' f (M1 x) = M1 <$> gconstraints' @c f x

instance GHasConstraints' c U1 where
  gconstraints' _ _ = pure U1

--------------------------------------------------------------------------------

type TraversalC (c :: * -> * -> Constraint) s t
  = forall f. Applicative f => (forall a b. c a b => a -> f b) -> s -> f t

type TraversalC' (c :: * -> Constraint) s
  = forall f. Applicative f => (forall a. c a => a -> f a) -> s -> f s

type LensLikeC c f s
  = (forall a. c a => a -> f a) -> s -> f s

class GHasConstraints (c :: * -> * -> Constraint) s t where
  gconstraints :: TraversalC c (s x) (t x)

instance
  ( GHasConstraints c l l'
  , GHasConstraints c r r'
  ) => GHasConstraints c (l :*: r) (l' :*: r') where
  gconstraints f (l :*: r)
    = (:*:) <$> gconstraints @c f l <*> gconstraints @c f r

instance
  ( GHasConstraints c l l'
  , GHasConstraints c r r'
  ) => GHasConstraints c (l :+: r) (l' :+: r') where
  gconstraints f (L1 l) = L1 <$> gconstraints @c f l
  gconstraints f (R1 r) = R1 <$> gconstraints @c f r

instance GHasConstraints c s t
  => GHasConstraints c (M1 i m s) (M1 i m t) where
  gconstraints f (M1 x) = M1 <$> gconstraints @c f x

instance GHasConstraints c U1 U1 where
  gconstraints _ _ = pure U1

instance GHasConstraints c V1 V1 where
  gconstraints _ = pure

instance c a b => GHasConstraints c (Rec0 a) (Rec0 b) where
  gconstraints f x = kIso f x

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

-- fuse constrained traversals
confusingC :: forall c f s. Applicative f => TraversalC' c s -> LensLikeC c f s
confusingC t = \f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
{-# INLINE confusingC #-}
