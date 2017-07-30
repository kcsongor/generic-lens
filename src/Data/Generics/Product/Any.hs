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
    --  $example
    HasAny (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Product.Fields
import Data.Generics.Product.Positions
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

-- |Records that have generic lenses.
class HasAny (sel :: k) a s | s sel k -> a where
  -- |A lens that focuses on a part of a product as identified by some
  --  selector. Currently supported selectors are field names, positions and
  --  unique types. Compatible with the lens package's 'Control.Lens.Lens'
  --  type.
  --
  --  >>> human ^. the @Int
  --  50
  --  >>> human ^. the @"name"
  --  "Tunyasz"
  --  >>> human ^. the @3
  --  "London"
  the :: Lens' s a

instance HasPosition i a s => HasAny i a s where
  the = position @i

instance HasField field a s => HasAny field a s where
  the = field @field

instance HasType a s => HasAny a a s where
  the = typed @a
