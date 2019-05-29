{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2018 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- For a good explanation of profunctors in Haskell see Dan Piponi's article:
--
-- <http://blog.sigfpe.com/2011/07/profunctors-in-haskell.html>
--
-- This module includes /unsafe/ composition operators that are useful in
-- practice when it comes to generating optimal core in GHC.
--
-- If you import this module you are taking upon yourself the obligation
-- that you will only call the operators with @#@ in their names with functions
-- that are operationally identity such as @newtype@ constructors or the field
-- accessor of a @newtype@.
--
-- If you are ever in doubt, use 'rmap' or 'lmap'.
----------------------------------------------------------------------------
module Data.Profunctor.Unsafe
  (
  -- * Profunctors
    Profunctor(..)
  ) where

import Data.Coerce
import Data.Tagged

infixr 9 #.
infixl 8 .#

----------------------------------------------------------------------------
-- Profunctors
----------------------------------------------------------------------------

-- | Formally, the class 'Profunctor' represents a profunctor
-- from @Hask@ -> @Hask@.
--
-- Intuitively it is a bifunctor where the first argument is contravariant
-- and the second argument is covariant.
--
-- You can define a 'Profunctor' by either defining 'dimap' or by defining both
-- 'lmap' and 'rmap'.
--
-- If you supply 'dimap', you should ensure that:
--
-- @'dimap' 'id' 'id' ≡ 'id'@
--
-- If you supply 'lmap' and 'rmap', ensure:
--
-- @
-- 'lmap' 'id' ≡ 'id'
-- 'rmap' 'id' ≡ 'id'
-- @
--
-- If you supply both, you should also ensure:
--
-- @'dimap' f g ≡ 'lmap' f '.' 'rmap' g@
--
-- These ensure by parametricity:
--
-- @
-- 'dimap' (f '.' g) (h '.' i) ≡ 'dimap' g h '.' 'dimap' f i
-- 'lmap' (f '.' g) ≡ 'lmap' g '.' 'lmap' f
-- 'rmap' (f '.' g) ≡ 'rmap' f '.' 'rmap' g
-- @
class Profunctor p where
  -- | Map over both arguments at the same time.
  --
  -- @'dimap' f g ≡ 'lmap' f '.' 'rmap' g@
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  {-# INLINE dimap #-}

  -- | Map the first argument contravariantly.
  --
  -- @'lmap' f ≡ 'dimap' f 'id'@
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  {-# INLINE lmap #-}

  -- | Map the second argument covariantly.
  --
  -- @'rmap' ≡ 'dimap' 'id'@
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id
  {-# INLINE rmap #-}

  -- | Strictly map the second argument argument
  -- covariantly with a function that is assumed
  -- operationally to be a cast, such as a newtype
  -- constructor.
  --
  -- /Note:/ This operation is explicitly /unsafe/
  -- since an implementation may choose to use
  -- 'unsafeCoerce' to implement this combinator
  -- and it has no way to validate that your function
  -- meets the requirements.
  --
  -- If you implement this combinator with
  -- 'unsafeCoerce', then you are taking upon yourself
  -- the obligation that you don't use GADT-like
  -- tricks to distinguish values.
  --
  -- If you import "Data.Profunctor.Unsafe" you are
  -- taking upon yourself the obligation that you
  -- will only call this with a first argument that is
  -- operationally identity.
  --
  -- The semantics of this function with respect to bottoms
  -- should match the default definition:
  --
  -- @('Profuctor.Unsafe.#.') ≡ \\_ -> \\p -> p \`seq\` 'rmap' 'coerce' p@
  (#.) :: forall a b c q. Coercible c b => q b c -> p a b -> p a c
  (#.) = \_ -> \p -> p `seq` rmap (coerce (id :: c -> c) :: b -> c) p
  {-# INLINE (#.) #-}

  -- | Strictly map the first argument argument
  -- contravariantly with a function that is assumed
  -- operationally to be a cast, such as a newtype
  -- constructor.
  --
  -- /Note:/ This operation is explicitly /unsafe/
  -- since an implementation may choose to use
  -- 'unsafeCoerce' to implement this combinator
  -- and it has no way to validate that your function
  -- meets the requirements.
  --
  -- If you implement this combinator with
  -- 'unsafeCoerce', then you are taking upon yourself
  -- the obligation that you don't use GADT-like
  -- tricks to distinguish values.
  --
  -- If you import "Data.Profunctor.Unsafe" you are
  -- taking upon yourself the obligation that you
  -- will only call this with a second argument that is
  -- operationally identity.
  --
  -- @('.#') ≡ \\p -> p \`seq\` \\f -> 'lmap' 'coerce' p@
  (.#) :: forall a b c q. Coercible b a => p b c -> q a b -> p a c
  (.#) = \p -> p `seq` \_ -> lmap (coerce (id :: b -> b) :: a -> b) p
  {-# INLINE (.#) #-}

  {-# MINIMAL dimap | (lmap, rmap) #-}

instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab
  {-# INLINE dimap #-}
  lmap = flip (.)
  {-# INLINE lmap #-}
  rmap = (.)
  {-# INLINE rmap #-}
  (#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
  (.#) pbc _ = coerce pbc
  {-# INLINE (#.) #-}
  {-# INLINE (.#) #-}

instance Profunctor Tagged where
  dimap _ f (Tagged s) = Tagged (f s)
  {-# INLINE dimap #-}
  lmap _ = retag
  {-# INLINE lmap #-}
  rmap = fmap
  {-# INLINE rmap #-}
  (#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
  {-# INLINE (#.) #-}
  Tagged s .# _ = Tagged s
  {-# INLINE (.#) #-}

