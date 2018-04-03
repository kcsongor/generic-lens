{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

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

module Data.Generics.Product.Param where

  -- ( HasParam (..)
  -- , Param (..)
  -- ) where

import           Unsafe.Coerce (unsafeCoerce)
import Data.Generics.Internal.Families.Changing
import Data.Generics.Internal.Simple
import Data.Generics.Internal.VL.Traversal
import Data.Generics.Product.Types
import GHC.TypeLits
import qualified GHC.Generics as G

import Data.Kind
import Data.Generics.Internal.VL.Iso

type family Error (b :: Bool) (expected :: Nat) (actual :: Nat) (s :: Type) :: Constraint where
  Error 'False _ _ _
    = ()

  Error 'True expected actual typ
    = TypeError
        (     'Text "Expected a type with at least "
        ':<>: 'ShowType (expected + 1)
        ':<>: 'Text " parameters, but "
        ':$$: 'ShowType typ
        ':<>: 'Text " only has "
        ':<>: 'ShowType actual
        )

class HasParam (i :: Nat) s t a b | i s b -> t, i s -> a, i t a -> s, i t -> b where
  param :: Traversal s t a b

instance
  ( a ~ GetParam s i
  , b ~ GetParam t i
  , t ~ PutParam s i b
  , s ~ PutParam t i a
  , GenericN s
  , GenericN t
  , GHasTypes (RepN s) (RepN t) (Param i Any) (Param i Any)
  , Error ((ArgCount s) <=? i) i (ArgCount s) s
  )
  => HasParam i s t a b where
  param = confusing (repIsoN . conv gtypes . paramIso @i)
    where conv :: LensLike f (RepN s) (RepN t) (Param i Any) (Param i Any) -> LensLike f (RepN s) (RepN t) (Param i a) (Param i b)
          conv = unsafeCoerce
  {-# INLINE param #-}

--------------------------------------------------------------------------------
-- TODO: reorganise these

data Any deriving G.Generic

type family Index (t :: k) (i :: Nat) :: k where
  Index (t _) i   = Index t (i + 1) (Param i Any)
  Index t _       = t

class Generic a => GenericN (a :: Type) where
  type RepN a :: Type
  toN     :: RepN a   -> a
  fromN   :: a        -> RepN a

instance Generic a => GenericN a where
  type RepN a = Rep (Index a 0)
  toN      = unsafeCoerce (to @a)
  {-# INLINE toN #-}
  fromN    = unsafeCoerce (from @a)
  {-# INLINE fromN #-}

paramIso :: forall i a b. Iso (Param i a) (Param i b) a b
paramIso = iso unParam Param
{-# INLINE paramIso #-}

repIsoN :: (GenericN s, GenericN t) => Iso s t (RepN s) (RepN t)
repIsoN = iso fromN toN
{-# INLINE repIsoN #-}

type family GetParam (t :: k) (i :: Nat) :: Type where
  GetParam (t a) 0 = a
  GetParam (t _) i = GetParam t (i - 1)

type family PutParam (t :: k) (i :: Nat) (b :: Type) :: k where
  PutParam (t _) 0 b = t b
  PutParam (t a) i b = (PutParam t (i - 1) b) a

