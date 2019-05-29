{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#endif
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}
----------------------------------------------------------------------------
-- |
-- Module     : Data.Tagged
-- Copyright  : 2009-2015 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------

module Data.Tagged
    (
    -- * Tagged values
      Tagged(..)
    , retag
    , untag
    , tagSelf
    , untagSelf
    , asTaggedTypeOf
    , witness
    -- * Conversion
    , proxy
    , unproxy
    , tagWith
    -- * Proxy methods GHC dropped
    , reproxy
    ) where

import Data.Proxy


-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
-- This can be used in place of the more traditional but less safe idiom of
-- passing in an undefined value with the type, because unlike an @(s -> b)@,
-- a @'Tagged' s b@ can't try to use the argument @s@ as a real value.
--
-- Moreover, you don't have to rely on the compiler to inline away the extra
-- argument, because the newtype is \"free\"
--
-- 'Tagged' has kind @k -> * -> *@ if the compiler supports @PolyKinds@, therefore
-- there is an extra @k@ showing in the instance haddocks that may cause confusion.
newtype Tagged s b = Tagged { unTagged :: b } deriving
  ( Eq, Ord, Bounded )

instance Show b => Show (Tagged s b) where
    showsPrec n (Tagged b) = showParen (n > 10) $
        showString "Tagged " .
        showsPrec 11 b

instance Read b => Read (Tagged s b) where
    readsPrec d = readParen (d > 10) $ \r ->
        [(Tagged a, t) | ("Tagged", s) <- lex r, (a, t) <- readsPrec 11 s]

instance Functor (Tagged s) where
    fmap f (Tagged x) = Tagged (f x)
    {-# INLINE fmap #-}

instance Applicative (Tagged s) where
    pure = Tagged
    {-# INLINE pure #-}
    Tagged f <*> Tagged x = Tagged (f x)
    {-# INLINE (<*>) #-}
    _ *> n = n
    {-# INLINE (*>) #-}

instance Monad (Tagged s) where
    return = pure
    {-# INLINE return #-}
    Tagged m >>= k = k m
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
-- | Some times you need to change the tag you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship between the
-- tags that you want to enforce, and define that combinator using 'retag'.
--
-- @
-- data Succ n
-- retagSucc :: 'Tagged' n a -> 'Tagged' (Succ n) a
-- retagSucc = 'retag'
-- @
retag :: Tagged s b -> Tagged t b
retag = Tagged . unTagged
{-# INLINE retag #-}

-- | Alias for 'unTagged'
untag :: Tagged s b -> b
untag = unTagged

-- | Tag a value with its own type.
tagSelf :: a -> Tagged a a
tagSelf = Tagged
{-# INLINE tagSelf #-}

-- | 'asTaggedTypeOf' is a type-restricted version of 'const'. It is usually used as an infix operator, and its typing forces its first argument (which is usually overloaded) to have the same type as the tag of the second.
asTaggedTypeOf :: s -> tagged s b -> s
asTaggedTypeOf = const
{-# INLINE asTaggedTypeOf #-}

witness :: Tagged a b -> a -> b
witness (Tagged b) _ = b
{-# INLINE witness #-}

-- | 'untagSelf' is a type-restricted version of 'untag'.
untagSelf :: Tagged a a -> a
untagSelf (Tagged x) = x
{-# INLINE untagSelf #-}

-- | Convert from a 'Tagged' representation to a representation
-- based on a 'Proxy'.
proxy :: Tagged s a -> proxy s -> a
proxy (Tagged x) _ = x
{-# INLINE proxy #-}

-- | Convert from a representation based on a 'Proxy' to a 'Tagged'
-- representation.
unproxy :: (Proxy s -> a) -> Tagged s a
unproxy f = Tagged (f Proxy)
{-# INLINE unproxy #-}

-- | Another way to convert a proxy to a tag.
tagWith :: proxy s -> a -> Tagged s a
tagWith _ = Tagged
{-# INLINE tagWith #-}

-- | Some times you need to change the proxy you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship
-- between the proxies that you want to enforce, and define that
-- combinator using 'reproxy'.
--
-- @
-- data Succ n
-- reproxySucc :: proxy n -> 'Proxy' (Succ n)
-- reproxySucc = 'reproxy'
-- @
reproxy :: proxy a -> Proxy b
reproxy _ = Proxy
