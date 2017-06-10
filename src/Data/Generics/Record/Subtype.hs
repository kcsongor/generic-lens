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
-- Module      :  Data.Generics.Record.Subtype
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structural subtype relationship between record types.
--
-- The running example in this module is the following two types:
--
-- @
--
--   module Test where
--
--   import GHC.Generics
--   import Data.Generics.Record
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
-----------------------------------------------------------------------------
module Data.Generics.Record.Subtype
  ( Subtype (..)
  , subtype
  ) where

import Data.Generics.Record.HasField
import Data.Generics.Record.Internal.Contains

import Data.Generics.Internal.Lens

import Data.Kind                (Type)
import GHC.Generics

-- |Structural subtype relationship
--
-- @sub@ is a (structural) `subtype' of @sup@, if its fields are a subset of
-- those of @sup@.
--
class Subtype sub sup where
  -- | Cast the more specific subtype to the more general supertype
  --
  -- >>> upcast human :: Animal
  -- Animal {name = "Tunyasz", age = 50}
  --
  upcast :: sub -> sup
  -- | Plug a smaller structure into a larger one
  --
  -- >>> smash (Animal "dog" 10) human
  -- Human {name = "dog", age = 10, address = "London"}
  smash  :: sup -> sub -> sub

-- | Instances are created by the compiler
instance (GSmash (Rep a) (Rep b), GUpcast (Rep a) (Rep b), Generic a, Generic b) => Subtype a b where
  upcast    = to . gupcast . from
  smash p b = to $ gsmash (from p) (from b)

-- | Structural subtype lens. Given a subtype relationship @sub :< sup@,
--   we can focus on the @sub@ structure of @sup@.
--
-- >>> human ^. subtype @Animal
-- Animal {name = "Tunyasz", age = 50}
--
-- >>> set (subtype @Animal) (Animal "dog" 10) human
-- Human {name = "dog", age = 10, address = "London"}
subtype :: forall sup sub. Subtype sub sup => Lens' sub sup
subtype f sub = fmap (flip smash sub) (f (upcast sub))

--------------------------------------------------------------------------------
-- * Generic upcasting

-- | Upcast 'sub to 'sup' (generic rep)
class GUpcast (sub :: Type -> Type) (sup :: Type -> Type) where
  gupcast :: sub p -> sup p

instance (GUpcast sub a, GUpcast sub b) => GUpcast sub (a :*: b) where
  gupcast rep = gupcast rep :*: gupcast rep

instance {-# OVERLAPPING #-} GHasField field sub t => GUpcast sub (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) where
  gupcast r = M1 (K1 (r ^. glabel @field))

instance GUpcast sub sup => GUpcast sub (M1 i c sup) where
  gupcast = M1 . gupcast

--------------------------------------------------------------------------------
-- * Generic smashing

class GSmash sub sup where
  gsmash :: sup p -> sub p -> sub p

instance (GSmash a sup, GSmash b sup) => GSmash (a :*: b) sup where
  gsmash rep (a :*: b) = gsmash rep a :*: gsmash rep b

instance {-# OVERLAPPING #-}
  ( leaf ~ (S1 ('MetaSel ('Just field) p f b) t)
  , GSmashLeaf leaf sup (Contains field sup)
  ) => GSmash (S1 ('MetaSel ('Just field) p f b) t) sup where
  gsmash = gsmashLeaf @_ @_ @(Contains field sup)

instance GSmash sub sup => GSmash (M1 i c sub) sup where
  gsmash sup (M1 sub) = M1 (gsmash sup sub)

class GSmashLeaf sub sup (w :: Maybe Type) where
  gsmashLeaf :: sup p -> sub p -> sub p

instance (GHasField field sup t) => GSmashLeaf (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) sup ('Just t) where
  gsmashLeaf sup (M1 (K1 _)) = M1 (K1 (sup ^. glabel @field))

instance GSmashLeaf (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) sup 'Nothing where
  gsmashLeaf _ = id
