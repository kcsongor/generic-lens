{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Product.Internal.Keyed
-- Copyright   : (C) 2018 Csongor Kiss
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
--------------------------------------------------------------------------------

module Data.Generics.Product.Internal.Keyed
  ( GHasKey (..)
  , GHasKey'
  ) where

import Data.Generics.Product.Internal.List
import Data.Kind
import GHC.Generics
import Data.Generics.Internal.Profunctor.Lens
import Data.Generics.Internal.Profunctor.Iso

class GHasKey (key :: k) (s :: Type -> Type) (t :: Type -> Type) a b | s key -> a, t key -> b where
  gkey :: Lens (s x) (t x) a b

type GHasKey' key s a = GHasKey key s s a a

instance (GHasKey key l l' a b, GHasKey key r r' a b) =>  GHasKey key (l :+: r) (l' :+: r') a b where
  gkey = sumIso . choosing (gkey @key) (gkey @key)

instance (GHasKey key f g a b) => GHasKey key (M1 D meta f) (M1 D meta g) a b where
  gkey = mLens . gkey @key

instance
  ( Elem as key i a
  , Elem bs key i b
  , IndexList i as bs a b
  , GIsList k f g as bs
  ) => GHasKey (key :: k) (M1 C meta f) (M1 C meta g) a b where
  gkey = mIso . glist @k . point @i

