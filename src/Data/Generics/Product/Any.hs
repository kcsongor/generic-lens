{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Any
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive a variety of lenses generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Any
  ( -- *Lenses
    --
    -- $setup
    HasAny (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Product.Fields
import Data.Generics.Product.Positions
import Data.Generics.Product.Typed

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.Lens
-- >>> :{
-- data Human = Human
--   { name    :: String
--   , age     :: Int
--   , address :: String
--   }
--   deriving (Generic, Show)
-- human :: Human
-- human = Human "Tunyasz" 50 "London"
-- :}

class HasAny (sel :: k) s t a b | s sel k -> a where
  -- |A lens that focuses on a part of a product as identified by some
  --  selector. Currently supported selectors are field names, positions and
  --  unique types. Compatible with the lens package's 'Control.Lens.Lens'
  --  type.
  --
  --  >>> human ^. the @Int
  --  50
  --
  --  >>> human ^. the @"name"
  --  "Tunyasz"
  --
  --  >>> human ^. the @3
  --  "London"
  the :: Lens s t a b

instance HasPosition i s t a b => HasAny i s t a b where
  the = position @i

instance HasField field s t a b => HasAny field s t a b where
  the = field @field

instance (HasType a s, t ~ s, a ~ b) => HasAny a s t a b where
  the = typed @a
