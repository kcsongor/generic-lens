{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Record.Internal.Contains
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Look up fields in the generic representation
--
-----------------------------------------------------------------------------
module Data.Generics.Record.Internal.Contains
  ( ContainsField
  ) where

import Data.Kind    (Type)
import GHC.Generics
import GHC.TypeLits

-- | Look up a record field by name in the generic representation, and return
--   its corresponding type, if exists.
type family ContainsField (field :: Symbol) f :: Maybe Type where
  ContainsField field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'Just t
  ContainsField field (f :*: g)
    = ContainsField field f <|> ContainsField field g
  ContainsField field (f :+: g)
    = ContainsField field f <&> ContainsField field g
  ContainsField field (S1 _ _)
    = 'Nothing
  ContainsField field (C1 m f)
    = ContainsField field f
  ContainsField field (D1 m f)
    = ContainsField field f
  ContainsField field (Rec0 _)
    = 'Nothing
  ContainsField field U1
    = 'Nothing
  ContainsField field V1
    = 'Nothing
  ContainsField x t
    = TypeError
        (     'ShowType t
        ':<>: 'Text " is not a valid GHC.Generics representation type"
        )

-- | Type-level alternative
type family (a :: Maybe k) <|> (b :: Maybe k) :: Maybe k where
  'Just x <|> _ = 'Just x
  _       <|> b = b

type family (a :: Maybe k) <&> (b :: Maybe k) :: Maybe k where
  'Just x <&> 'Just x = 'Just x
  _       <&> _       = 'Nothing
