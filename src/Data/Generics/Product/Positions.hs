{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Positions
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive positional product type getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Positions
  ( -- *Lenses

    --  $example
    HasPosition (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Product.Internal.Positions

import Data.Kind      (Constraint, Type)
import Data.Type.Bool (type (&&))
import GHC.Generics
import GHC.TypeLits   (type (<=?),  Nat, TypeError, ErrorMessage(..))

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

-- |Records that have a field at a given position.
class HasPosition (i :: Nat) a s | s i -> a where
  -- |A lens that focuses on a field at a given position. Compatible with the
  --  lens package's 'Control.Lens.Lens' type.
  --
  --  >>> human ^. position @1
  --  "Tunyasz"
  --  >>> human & position @2 .~ "Berlin"
  --  Human {name = "Tunyasz", age = 50, address = "Berlin"}
  position :: Lens' s a
  position f s
    = fmap (flip (setPosition @i) s) (f (getPosition @i s))
    -- = fmap (setPosition s) (f (getPosition s))

  -- |Get positional field
  --
  -- >>> getPosition @1 human
  -- "Tunyasz"
  getPosition :: s -> a
  getPosition s = s ^. position @i

  -- |Set positional field
  --
  -- >>> setPosition @2 (setField @1 "Tamas" human) 30
  -- Human "Tamas" 30 "London"
  setPosition :: a -> s -> s
  setPosition = set (position @i)

  {-# MINIMAL position | setPosition, getPosition #-}

instance
  ( Generic s
  , ErrorUnless i s (0 <? i && i <=? Size (Rep s))
  , GHasPosition 1 i (Rep s) a
  ) => HasPosition i a s where

  position = ravel (repLens . gposition @1 @i)

type family ErrorUnless (i :: Nat) (s :: Type) (hasP :: Bool) :: Constraint where
  ErrorUnless i s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field at position "
        ':<>: 'ShowType i
        )

  ErrorUnless _ _ 'True
    = ()
