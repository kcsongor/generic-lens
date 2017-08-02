{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Subtype
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structural subtype relationships between product types.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Subtype
  ( -- *Lenses
    --
    --  $example
    Subtype (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens
import Data.Generics.Product.Fields

import Data.Kind (Type)
import GHC.Generics

--  $example
--  @
--     module Example where
--
--     import Data.Generics.Product
--     import GHC.Generics
--
--     data Human = Human
--       { name    :: String
--       , age     :: Int
--       , address :: String
--       } deriving (Generic, Show)
--
--     data Animal = Animal
--       { name    :: String
--       , age     :: Int
--       } deriving (Generic, Show)
--
--     human :: Human
--     human = Human \"Tunyasz\" 50 \"London\"
--  @

-- |Structural subtype relationship
--
-- @sub@ is a (structural) `subtype' of @sup@, if its fields are a subset of
-- those of @sup@.
--
class Subtype sup sub where
  -- |Structural subtype lens. Given a subtype relationship @sub :< sup@,
  --  we can focus on the @sub@ structure of @sup@.
  --
  -- >>> human ^. super @Animal
  -- Animal {name = "Tunyasz", age = 50}
  --
  -- >>> set (super @Animal) (Animal "dog" 10) human
  -- Human {name = "dog", age = 10, address = "London"}
  super  :: Lens' sub sup
  super f sub
    = fmap (`smash` sub) (f (upcast sub))

  -- |Cast the more specific subtype to the more general supertype
  --
  -- >>> upcast human :: Animal
  -- Animal {name = "Tunyasz", age = 50}
  --
  upcast :: sub -> sup
  upcast s = s ^. super @sup

  -- |Plug a smaller structure into a larger one
  --
  -- >>> smash (Animal "dog" 10) human
  -- Human {name = "dog", age = 10, address = "London"}
  smash  :: sup -> sub -> sub
  smash = set (super @sup)

  {-# MINIMAL super | smash, upcast #-}

instance
  ( GSmash (Rep a) (Rep b)
  , GUpcast (Rep a) (Rep b)
  , Generic a
  , Generic b
  ) => Subtype b a where
    smash p b = to $ gsmash (from p) (from b)
    upcast    = to . gupcast . from

--------------------------------------------------------------------------------
-- * Generic upcasting

class GUpcast (sub :: Type -> Type) (sup :: Type -> Type) where
  gupcast :: sub p -> sup p

instance (GUpcast sub a, GUpcast sub b) => GUpcast sub (a :*: b) where
  gupcast rep = gupcast rep :*: gupcast rep

instance
  GHasField field sub t
  => GUpcast sub (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) where

  gupcast r = M1 (K1 (r ^. gfield @field))

instance GUpcast sub sup => GUpcast sub (C1 c sup) where
  gupcast = M1 . gupcast

instance GUpcast sub sup => GUpcast sub (D1 c sup) where
  gupcast = M1 . gupcast

--------------------------------------------------------------------------------
-- * Generic smashing

class GSmash sub sup where
  gsmash :: sup p -> sub p -> sub p

instance (GSmash a sup, GSmash b sup) => GSmash (a :*: b) sup where
  gsmash rep (a :*: b) = gsmash rep a :*: gsmash rep b

instance
  ( leaf ~ (S1 ('MetaSel ('Just field) p f b) t)
  , GSmashLeaf leaf sup (HasTotalFieldP field sup)
  ) => GSmash (S1 ('MetaSel ('Just field) p f b) t) sup where

  gsmash = gsmashLeaf @_ @_ @(HasTotalFieldP field sup)

instance GSmash sub sup => GSmash (C1 c sub) sup where
  gsmash sup (M1 sub) = M1 (gsmash sup sub)

instance GSmash sub sup => GSmash (D1 c sub) sup where
  gsmash sup (M1 sub) = M1 (gsmash sup sub)

class GSmashLeaf sub sup (w :: Bool) where
  gsmashLeaf :: sup p -> sub p -> sub p

instance
  GHasField field sup t
  => GSmashLeaf (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) sup 'True where
  gsmashLeaf sup _ = M1 (K1 (sup ^. gfield @field))

instance GSmashLeaf (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) sup 'False where
  gsmashLeaf _ = id
