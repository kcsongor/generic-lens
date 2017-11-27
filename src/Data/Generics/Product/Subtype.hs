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
    --  $setup
    Subtype (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Internal.Void
import Data.Generics.Product.Internal.Subtype

import GHC.Generics (Generic (Rep, to, from) )

-- $setup
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XDuplicateRecordFields
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.Lens
-- >>> :{
-- data Human = Human
--   { name    :: String
--   , age     :: Int
--   , address :: String
--   }
--   deriving (Generic, Show)
-- data Animal = Animal
--   { name    :: String
--   , age     :: Int
--   }
--   deriving (Generic, Show)
-- human :: Human
-- human = Human "Tunyasz" 50 "London"
-- :}

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

-- TODO: improve type error by showing a diff of fields
instance
  ( Generic a
  , Generic b
  , GSmash (Rep a) (Rep b)
  , GUpcast (Rep a) (Rep b)
  ) => Subtype b a where
    smash p b = to $ gsmash (from p) (from b)
    upcast    = to . gupcast . from

-- See Note [Uncluttering type signatures]
instance {-# OVERLAPPING #-} Subtype a Void where
  super = undefined
instance {-# OVERLAPPING #-} Subtype Void a where
  super = undefined
