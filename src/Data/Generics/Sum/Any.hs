{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Any
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive a variety of prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Any
  ( -- *Prisms
    --
    --  $example
    AsAny (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Sum.Constructors
import Data.Generics.Sum.Typed

--  $example
--  @
--    module Example where
--
--    import Data.Generics.Sum
--    import GHC.Generics
--
--    data Animal
--      = Dog Dog
--      | Cat Name Age
--      | Duck Age
--      deriving (Generic, Show)
--
--    data Dog = MkDog
--      { name :: Name
--      , age  :: Age
--      }
--      deriving (Generic, Show)
--
--    type Name = String
--    type Age  = Int
--
--    dog, cat, duck :: Animal
--
--    dog = Dog (MkDog "Shep" 3)
--    cat = Cat "Mog" 5
--    duck = Duck 2
--  @

-- |Sums that have generic prisms.
class AsAny (sel :: k) a s | s sel k -> a where
  -- |A prism that projects a sum as identified by some selector. Currently
  --  supported selectors are constructor names and unique types. Compatible
  --  with the lens package's 'Control.Lens.Prism' type.
  --
  --  >>> dog ^? _As @"Dog"
  --  Just (MkDog {name = "Shep", age = 3})
  --  >>> dog ^? _As @Dog
  --  Just (MkDog {name = "Shep", age = 3})
  --  >>> dog ^? _As @"Cat"
  --  Nothing
  --  >>> cat ^? _As @(Name, Age)
  --  Just ("Mog",5)
  --  >>> cat ^? _As @"Cat"
  --  Just ("Mog",5)
  --  >>> _As @"Cat" # ("Garfield", 6) :: Animal
  --  Cat ("Garfield",6)
  --  >>> duck ^? _As @Age
  --  Just 2
  _As :: Prism' s a

instance AsConstructor ctor a s => AsAny ctor a s where
  _As = _Ctor @ctor

instance AsType a s => AsAny a a s where
  _As = _Typed @a
