{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  ( Contains
  ) where

import Data.Kind                (Type)
import GHC.Generics

import GHC.TypeLits

-- | Look up a record field by name in the generic representation, and return
--   its corresponding type, if exists.
type family Contains (field :: Symbol) f :: Maybe Type where
  Contains field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'Just t
  Contains field (f :*: g)
    = Contains field f <|> Contains field g
  Contains field (S1 _ _)
    = 'Nothing
  Contains field (C1 m f)
    = Contains field f
  Contains field (D1 m f)
    = Contains field f
  Contains field (Rec0 _)
    = 'Nothing
  Contains field U1
    = 'Nothing
  Contains field V1
    = 'Nothing
  Contains x t = TypeError ('ShowType t)

-- | Type-level alternative
type family (a :: Maybe k) <|> (b :: Maybe k) :: Maybe k where
  'Just x <|> _  = 'Just x
  _ <|> b = b

