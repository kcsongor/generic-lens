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
-- Module      :  Data.Generics.Product.HasFieldAt
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive product items lenses generically.
--
-- @
--
--   module Example where
--
--   import GHC.Generics
--   import Data.Generics.Product
--
--   data Human = Human String Int String
--     deriving (Generic, Show)
--
--   human :: Human
--   human = Human \"Tunyasz\" 50 \"London\"
--
-- @
--
-----------------------------------------------------------------------------

module Data.Generics.Product.HasFieldAt
  ( HasFieldAt (..)
    -- * Getter and setter functions
  , getFieldAt
  , setFieldAt
  ) where

import Data.Generics.Internal.Lens

import GHC.TypeLits
import Data.Kind                (Type)
import GHC.Generics

-- | Get positional field
--
-- >>> getFieldAt @1 human
-- "Tunyasz"
getFieldAt :: forall index a s. HasFieldAt index a s => s -> a
getFieldAt s = s ^. itemAt @index

-- | Set positional field
--
-- >>> setFieldAt @2 (setField @1 "Tamas" human) 30
-- Human "Tamas" 30 "London"
setFieldAt :: forall index a s. HasFieldAt index a s => a -> s -> s
setFieldAt = set (itemAt @index)

-- | Types that have a field at given position.
class HasFieldAt (index :: Nat) a s | s index -> a where
  -- ^ Lens focusing on a field at a given index.
  --   Compatible with the lens package.
  --
  -- >>> human & itemAt @1 .~ "Tamas"
  -- Human "Tamas" 50 "London"
  itemAt :: Lens' s a

-- | Fields are indexed from BaseIndex upwards.
type BaseIndex = 1

-- | Instances are generated on the fly for all types that have the required
--   positional field.
instance
  ( Generic s
  , GHasFieldAt BaseIndex index (Rep s) a
  ) => HasFieldAt index a s where
  itemAt =  repIso . gItemAt @BaseIndex @index

--------------------------------------------------------------------------------
type family Not (b :: Bool) where
  Not 'True = 'False
  Not 'False = 'True

type family (<) (x :: Nat) (y :: Nat) :: Bool where
  x < y = Not (y <=? x)

type family If (c :: Bool) (x :: k) (y :: k) :: k where
  If 'True  x _ = x
  If 'False _ y = y

type family Min (a :: Nat) (b :: Nat) :: Nat where
  Min a b = If (a < b) a b

-- | Returns the count of terminal nodes in the generic representation.
type family Size f :: Nat where
  Size (D1 m f)
    = Size f
  Size (f :*: g)
    = Size f + Size g
  Size (f :+: g)
    = Min (Size f) (Size g)
  Size (C1 m f)
    = Size f
  Size t = 1


data Choice = GoLeft | GoRight

type family Search offset index f g :: Choice where
  Search offset index f g =
    If (index < offset)
      (TypeError ('Text "Type does not have specified field."))
      (If (index < (Size f + offset))
        'GoLeft
        (If (index < (Size f + Size g + offset))
          'GoRight
          (TypeError ('Text "Type does not have specified field."))))

-- | If traversing to the left, offset does not change.
--   If traversing to the right, offset is incremented by size of left subtree.
type family NewOffset (offset :: Nat) (choice :: Choice) (size :: Nat) :: Nat where
  NewOffset n 'GoLeft _ = n
  NewOffset n 'GoRight s = n + s

--------------------------------------------------------------------------------

class GHasFieldAtProd offset index a b ret (w :: Choice) | offset index a b w -> ret where
  prodItemAt :: Lens' ((a :*: b) x) ret

instance (GHasFieldAt offset index f ret) => GHasFieldAtProd offset index f g ret 'GoLeft where
  prodItemAt = first . gItemAt @offset @index

instance (GHasFieldAt offset index g ret) => GHasFieldAtProd offset index f g ret 'GoRight where
  prodItemAt = second . gItemAt @offset @index


-- | Like 'HasFieldAt', but on the generic representation
class GHasFieldAt (offset :: Nat) (index :: Nat) (s :: Type -> Type) a | offset index s -> a where
  gItemAt :: Lens' (s x) a

instance
    ( choice ~ (Search offset index s s')
    , offset' ~ NewOffset offset choice (Size s)
    , GHasFieldAtProd offset' index s s' a choice
    )
    => GHasFieldAt offset index (s :*: s') a where
  gItemAt = prodItemAt @offset' @index @_ @_ @_ @choice

instance
  ( GHasFieldAt offset index s a
  , GHasFieldAt offset index s' a
  )
  => GHasFieldAt offset index (s :+: s') a where
  gItemAt = combine (gItemAt @offset @index @s) (gItemAt @offset @index @s')

instance GHasFieldAt offset index (K1 R a) a where
  gItemAt f (K1 x) = fmap K1 (f x)

instance GHasFieldAt n n (S1 ('MetaSel m p f b) (Rec0 a)) a where
  gItemAt = mIso . gItemAt @n @n

instance GHasFieldAt offset index s a => GHasFieldAt offset index (M1 D c s) a where
  gItemAt = mIso . gItemAt @offset @index

instance GHasFieldAt offset index s a => GHasFieldAt offset index (M1 C c s) a where
  gItemAt = mIso . gItemAt @offset @index

