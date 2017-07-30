{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Record.HasField
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive a variety of record lenses generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Any
  ( --  * Lenses
    --
    --    $example
    HasAny (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Product.Fields
import Data.Generics.Product.Numbered
import Data.Generics.Product.Typed

--  $example
--  @
--    module Example where
--
--    import Data.Generics.Product
--    import GHC.Generics
--
--    data Human = Human
--      { name    :: String
--      , age     :: Int
--      , address :: String
--      }
--      deriving (Generic, Show)
--
--    human :: Human
--    human = Human \"Tunyasz\" 50 \"London\"
--  @

--  | Records that have generic lenses.
class HasAny (sel :: k) a s | s sel k -> a where
  --  | A lens that focuses on a part of a product as identified by some
  --    selector. Currently supported selectors are field names, positions and
  --    unique types. Compatible with the lens package's 'Control.Lens.Lens'
  --    type.
  --
  --    >>> human ^. its @Int
  --    50
  --    >>> human ^. its @"name"
  --    "Tunyasz"
  --    >>> human ^. its @3
  --    "London"
  its :: Lens' s a

instance HasPosition i a s => HasAny i a s where
  its = numbered @i

instance HasField field a s => HasAny field a s where
  its = field @field

instance HasType a s => HasAny a a s where
  its = typed @a
