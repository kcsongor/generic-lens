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

    -- *Internals
  , GHasPosition (..)
  ) where

import Data.Generics.Internal.Lens

import Data.Kind
import GHC.Generics
import GHC.TypeLits

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
  , ErrorUnlessTrue i s (0 <? i && i <=? Size (Rep s))
  , GHasPosition 1 i (Rep s) a
  ) => HasPosition i a s where

  position = repIso . gposition @1 @i

type family ErrorUnlessTrue (i :: Nat) (s :: Type) (hasP :: Bool) :: Constraint where
  ErrorUnlessTrue i s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field at position "
        ':<>: 'ShowType i
        )

  ErrorUnlessTrue _ _ 'True
    = ()

-- |As 'HasPosition' but over generic representations as defined by
--  "GHC.Generics".
class GHasPosition (offset :: Nat) (i :: Nat) (f :: Type -> Type) a | offset i f -> a where
  gposition :: Lens' (f x) a

instance GHasPosition i i (S1 meta (Rec0 a)) a where
  gposition = mIso . gposition @i @i

instance GHasPosition offset i (K1 R a) a where
  gposition f (K1 x) = fmap K1 (f x)

instance GHasPosition offset i f a => GHasPosition offset i (M1 D meta f) a where
  gposition = mIso . gposition @offset @i

instance GHasPosition offset i f a => GHasPosition offset i (M1 C meta f) a where
  gposition = mIso . gposition @offset @i

instance
  ( goLeft  ~ (i <? (offset + Size l))
  , offset' ~ (If goLeft offset (offset + Size l))
  , GProductHasPosition offset' i l r a goLeft
  ) => GHasPosition offset i (l :*: r) a where

  gposition = gproductPosition @offset' @i @_ @_ @_ @goLeft


class GProductHasPosition (offset :: Nat) (i :: Nat) l r a (left :: Bool) | offset i l r left -> a where
  gproductPosition :: Lens' ((l :*: r) x) a

instance GHasPosition offset i l a => GProductHasPosition offset i l r a 'True where
  gproductPosition = first . gposition @offset @i

instance GHasPosition offset i r a => GProductHasPosition offset i l r a 'False where
  gproductPosition = second . gposition @offset @i

type family Size f :: Nat where
  Size (l :*: r)
    = Size l + Size r
  Size (l :+: r)
    = Min (Size l) (Size r)
  Size (D1 meta f)
    = Size f
  Size (C1 meta f)
    = Size f
  Size f
    = 1

--------------------------------------------------------------------------------

type family Not (b :: Bool) where
  Not 'True = 'False
  Not 'False = 'True

type family (<?) (x :: Nat) (y :: Nat) :: Bool where
  x <? y = Not (y <=? x)

infixl 4 <?

type family If (c :: Bool) (x :: k) (y :: k) :: k where
  If 'True  x _ = x
  If 'False _ y = y

type family Min (a :: Nat) (b :: Nat) :: Nat where
  Min a b = If (a <? b) a b

type family (a :: Bool) && (b :: Bool) :: Bool where
  'True && 'True = 'True
  _     && _     = 'False

infix 3 &&
