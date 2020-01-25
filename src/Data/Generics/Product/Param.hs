{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Product.Param
-- Copyright   : (C) 2020 Csongor Kiss
-- License     : BSD3
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Derive traversals over type parameters
--
--------------------------------------------------------------------------------

module Data.Generics.Product.Param
  ( Rec (Rec) -- TODO: this has to be re-exported so the constructor is visible for Coercible... is there a better way?
  , HasParam (..)
  , Param (..)
  ) where

import GHC.TypeLits
import Data.Generics.Internal.Families.Changing
import Data.Generics.Internal.VL.Traversal

import GHC.Generics
import Data.Kind
import Data.Generics.Internal.VL.Iso
import Data.Generics.Internal.GenericN
import Data.Generics.Internal.Errors

import Data.Generics.Product.Types
import Data.Generics.Internal.Void

class HasParam (p :: Nat) s t a b | p t a -> s, p s b -> t, p s -> a, p t -> b where
  param :: Traversal s t a b

instance
  ( GenericN s
  , GenericN t
  -- TODO: merge the old 'Changing' code with 'GenericN'
  , Defined (Rep s)
    (NoGeneric s
      '[ 'Text "arising from a generic traversal of the type parameter at position " ':<>: QuoteType n
       , 'Text "of type " ':<>: QuoteType a ':<>: 'Text " in " ':<>: QuoteType s
       ])
    (() :: Constraint)
  , s ~ Infer t (P n b 'PTag) a
  , t ~ Infer s (P n a 'PTag) b
  , Error ((ArgCount s) <=? n) n (ArgCount s) s
  , a ~ ArgAt s n
  , b ~ ArgAt t n
  , GHasTypes ChGeneric (RepN s) (RepN t) (Param n a) (Param n b)
  ) => HasParam n s t a b where

  param = confusing (repIsoN . gtypes_ @ChGeneric . paramIso @n)
  {-# INLINE param #-}

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

instance {-# OVERLAPPING #-} HasParam p (Void1 a) (Void1 b) a b where
  param = undefined
