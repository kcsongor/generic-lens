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
-- Module      :  Data.Generic.Record.Subtype
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structural subtyping
--
-----------------------------------------------------------------------------
module Data.Generic.Record.Subtype
  ( Subtype (..)
  ) where

import Data.Generic.Record.HasField
import Data.Generic.Record.Lens

import Data.Kind                (Type)
import GHC.Generics

-- |Structural subtype relationship
--
-- @
--
--   module Test where
--
--   import GHC.Generics
--   import Data.Generic.Record
--
--   data Human = Human
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     } deriving (Generic, Show)
--
--   data Animal = Animal
--     { name    :: String
--     , age     :: Int
--     } deriving (Generic, Show)
--
--    human :: Human
--    human = Human \"Tunyasz\" 50 \"London\"
--
-- @
--
--
-- >>> human
-- Human {name = "Tunyasz", age = 50, address = "London"}
--
-- >>> upcast human :: Animal
-- Animal {name = "Tunyasz", age = 50}
--
class Subtype sub sup where
  -- | Cast the more specific subtype to the more general supertype
  upcast :: sub -> sup

-- | Instances are created by the compiler
instance (Convert (Rep a) (Rep b), Generic a, Generic b) => Subtype a b where
  upcast = to . convert . from

-- | Convert 'rep' into 'f'
class Convert (rep :: Type -> Type) (f :: Type -> Type) where
  convert :: rep p -> f p

instance (Convert rep a, Convert rep b) => Convert rep (a :*: b) where
  convert rep = convert rep :*: convert rep

instance {-# OVERLAPPING #-} GHasField field rep t => Convert rep (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) where
  convert rep = M1 (K1 (rep ^. glabel @field))

instance  Convert rep f => Convert rep (M1 i c f) where
  convert = M1 . convert
