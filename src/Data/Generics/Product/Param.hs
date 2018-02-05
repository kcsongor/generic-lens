{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Product.Param
-- Copyright   : (C) 2018 Csongor Kiss
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- Derive traversal over type parameters
--
--------------------------------------------------------------------------------

module Data.Generics.Product.Param
  ( Rec (Rec) -- TODO: this has to be re-exported so the constructor is visible for Coercible... is there a better way?
  , HasParam (..)
  ) where

import GHC.TypeLits (Nat)
import Data.Generics.Internal.Void
import Data.Generics.Internal.GenericN
import Data.Generics.Internal.Families.Changing
import Data.Generics.Product.Internal.Param
import Data.Generics.Internal.VL.Traversal

class HasParam (p :: Nat) s t a b | p t a b -> s, p s a b -> t where
  param :: Applicative g => (a -> g b) -> s -> g t

instance
  ( GenericN s
  , GenericN t
  -- TODO: merge the old 'Changing' code with 'GenericN'
  , s ~ Infer t (P n b 'PTag) a
  , t ~ Infer s (P n a 'PTag) b
  , GHasParam n (RepN s) (RepN t) a b
  ) => HasParam n s t a b where

  param = confusing (\f s -> toN <$> gparam @n f (fromN s))
  {-# INLINE param #-}

instance {-# OVERLAPPING #-} HasParam p (Void1 a) (Void1 b) a b where
  param = undefined

