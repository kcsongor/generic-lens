{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- This module generates implementations of the 'traverse' operation which
-- make it possible for GHC to optimize away the GHC.Generics value
-- representation.
module Data.Generics.Product.Boggle
  (
  -- * Generic operations
  genericTraverse,
  -- * Implementation details
  GTraversable(gtraverse)
  ) where

import Boggle
import Data.Generics.Internal.Lens
import GHC.Generics     (Generic1, Rep1, to1, from1, (:*:)(..), (:+:)(..),
                         (:.:)(..),
                         M1(..), K1(..), Rec1(..), Par1(..), U1(..), V1)
import GHC.Exts         (inline)

-- NOTE: genericTraversal an gtraverse must be explicitly marked
-- for inlining as they need to inline across module boundaries
-- for GHC to optimize away the generics representation. The other
-- functions don't *need* to be marked for inlining because GHC
-- does figure it out, but it's better to be explicit about our
-- intention here than to rely on the optimizer any more than
-- we already are.

-- | Implementation of 'traverse' for any instance of 'Generic1'.
genericTraverse ::
  (Generic1 t, GTraversable (Rep1 t)) => Traversal (t a) (t b) a b
genericTraverse f x = lowerBoggle (to1 <$> gtraverse f (from1 x))
{-# INLINE genericTraverse #-}

-- | The 'GTraversable' class has a method for traversing a generic
-- structure. This function is not quite the same as 'traverse' because
-- it uses a particular transformation on the underlying applicative functor.
class GTraversable t where
  gtraverse :: Applicative f => (a -> f b) -> t a -> Boggle f (t b)

instance GTraversable f => GTraversable (M1 i c f) where
  gtraverse f (M1 x) = M1 <$> gtraverse f x
  {-# INLINE gtraverse #-}

instance (GTraversable f, GTraversable g) => GTraversable (f :+: g) where
  gtraverse f (L1 x) = L1 <$> gtraverse f x
  gtraverse f (R1 x) = R1 <$> gtraverse f x
  {-# INLINE gtraverse #-}

instance (GTraversable f, GTraversable g) => GTraversable (f :*: g) where
  gtraverse f (x :*: y) = (:*:) <$> gtraverse f x <*> gtraverse f y
  {-# INLINE gtraverse #-}

instance (Traversable f, GTraversable g) => GTraversable (f :.: g) where
  gtraverse f (Comp1 x) = Comp1 <$> inline traverse (gtraverse f) x
  {-# INLINE gtraverse #-}

instance GTraversable U1 where
  gtraverse _ _ = pure U1
  {-# INLINE gtraverse #-}

instance GTraversable V1 where
  gtraverse _ v = v `seq` error "GTraversal/V1: gtraverse"
  {-# INLINE gtraverse #-}

instance GTraversable (K1 i a) where
  gtraverse _ (K1 x) = pure (K1 x)
  {-# INLINE gtraverse #-}

instance GTraversable Par1 where
  gtraverse f (Par1 x) = Par1 <$> liftBoggle (f x)
  {-# INLINE gtraverse #-}

instance Traversable t => GTraversable (Rec1 t) where
  gtraverse f (Rec1 x) = Rec1 <$> liftBoggle (traverse f x)
  {-# INLINE gtraverse #-}
