{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Internal.Subtype
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structural subtype relationships between sum types.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Internal.Subtype
  ( GAsSubtype (..)
  ) where

import Data.Generics.Internal.HList
import Data.Generics.Sum.Internal.Typed

import Data.Kind
import GHC.Generics

-- |As 'AsSubtype' but over generic representations as defined by
--  "GHC.Generics".
class GAsSubtype (subf :: Type -> Type) (supf :: Type -> Type) where
  ginjectSub  :: subf x -> supf x
  gprojectSub :: supf x -> Either (supf x) (subf x)

instance
  ( GAsSubtype l supf
  , GAsSubtype r supf
  ) => GAsSubtype (l :+: r) supf where

  ginjectSub x = case x of
    L1 l -> ginjectSub l
    R1 r -> ginjectSub r
  gprojectSub x
    = case gprojectSub x of
        Right y -> Right (L1 y)
        _ -> fmap R1 (gprojectSub x)

instance
  ( GAsType supf a
  , GCollectible subf as
  , ListTuple a as
  ) => GAsSubtype (C1 meta subf) supf where

  ginjectSub
    = ginjectTyped . listToTuple . gtoCollection . unM1
  gprojectSub
    = fmap (M1 . gfromCollection . tupleToList) . gprojectTyped

instance GAsType supf a => GAsSubtype (S1 meta (Rec0 a)) supf where
  ginjectSub
    = ginjectTyped @supf . unK1 . unM1
  gprojectSub
    = fmap (M1 . K1) . gprojectTyped @supf

instance GAsSubtype subf supf => GAsSubtype (D1 meta subf) supf where
  ginjectSub
    = ginjectSub . unM1
  gprojectSub
    = fmap M1 . gprojectSub
