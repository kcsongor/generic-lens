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
-- Module      :  Data.Generic.Product.HasFieldAt
-- Copyright   :  (C) 2017 Toby Shaw
-- License     :  BSD3
-- Maintainer  :  Toby Shaw <toby.shaw.96@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive product items lenses generically.
--
-----------------------------------------------------------------------------

module Data.Generic.Product.HasFieldAt
  ( -- * Lens
    -- $example
    HasFieldAt (..)
    -- * Getter and setter functions
  , getFieldAt
  , setFieldAt
  ) where

import Data.Generic.Internal.Lens

import GHC.TypeLits
import Data.Kind                (Type)
import GHC.Generics

-- $example
-- @
--
--   module Example where
--
--   import GHC.Generics
--   import Data.Generic.Product
--
--   data Human = Human String Int String
--     deriving (Generic, Show)
--
--   human :: Human
--   human = Human \"Tunyasz\" 50 \"London\"
--
-- @

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
  -- @
  --  type Lens' s a
  --    = forall f. Functor f => (a -> f a) -> s -> f s
  -- @
  --
  -- >>> human & label @1 .~ "Tamas"
  -- Human "Tamas" 50 "London"
  itemAt :: Lens' s a

-- | Fields are indexed from BaseIndex upwards.
type BaseIndex = 1

-- | Instances are generated on the fly for all types that have the required
--   positional field.
instance
  ( Generic s
  , ContainsAt BaseIndex index (Rep s) ~ 'True -- this is needed for the fundep
  , GHasFieldAt BaseIndex index (Rep s) a
  ) => HasFieldAt index a s where
  itemAt =  repIso . gItemAt @BaseIndex @index

data Choice = GoLeft | GoRight

class GHasFieldAtProd offset index a b ret (w :: Choice) | offset index a b w -> ret where
  prodItemAt :: Lens' ((a :*: b) x) ret

instance (GHasFieldAt offset index f ret) => GHasFieldAtProd offset index f g ret 'GoLeft where
  prodItemAt = first . gItemAt @offset @index

instance (GHasFieldAt offset index g ret) => GHasFieldAtProd offset index f g ret 'GoRight where
  prodItemAt = second . gItemAt @offset @index

--------------------------------------------------------------------------------

type family Search offset index f g :: Choice where
  Search offset index f g = Choose (ContainsAt offset index f) (ContainsAt (offset + Size f) index g)

type family Choose f g :: Choice where
  Choose 'True _ = 'GoLeft 
  Choose _ 'True = 'GoRight
  Choose _ _ = TypeError ('Text "Could not find type") 

-- | Try find a field by position in the generic representation.
type family ContainsAt (offset :: Nat) (index :: Nat) f :: Bool where
  ContainsAt offset index (D1 m f)
    = ContainsAt offset index f
  ContainsAt n n (S1 _ _)
    = 'True
  ContainsAt _ _ (S1 _ _)
    = 'False
  ContainsAt offset index  (f :*: g)
    = ContainsAt offset index f || ContainsAt (Size f + offset) index g
  ContainsAt offset index (C1 m f)
    = ContainsAt offset index f
  ContainsAt offset index (Rec0 _)
    = 'False
  ContainsAt offset index  U1
    = 'False
  ContainsAt offset index V1
    = 'False
  ContainsAt offset index t = TypeError ('ShowType offset ':<>: 'Text " " ':<>: 'ShowType index)

-- | Returns the count of terminal nodes in the generic representation.
type family Size f :: Nat where
  Size (D1 m f)
    = Size f
  Size (f :*: g)
    = Size f + Size g
  Size (C1 m f)
    = Size f
  Size t = 1

-- | If traversing to the left, offset does not change.
--   If traversing to the right, offset is incremented by size of left subtree.
type family Offset (offset :: Nat) (choice :: Choice) (size :: Nat) :: Nat where
  Offset n 'GoLeft _ = n
  Offset n 'GoRight s = n + s

-- | Type-level or
type family (a :: Bool) || (b :: Bool) :: Bool where
  'True || _  = 'True
  _ || b = b

--------------------------------------------------------------------------------

-- | Like 'HasFieldAt', but on the generic representation
class GHasFieldAt (offset :: Nat) (index :: Nat) (s :: Type -> Type) a | offset index s -> a where
  gItemAt :: Lens' (s x) a

instance
    ( choice ~ (Search offset index s s')
    , offset' ~ Offset offset choice (Size s)
    , GHasFieldAtProd offset' index s s' a choice 
    ) 
    => GHasFieldAt offset index (s :*: s') a where
  gItemAt = prodItemAt @offset' @index @_ @_ @_ @choice

instance GHasFieldAt offset index (K1 R a) a where
  gItemAt f (K1 x) = fmap K1 (f x)

instance GHasFieldAt n n (S1 ('MetaSel m p f b) (Rec0 a)) a where
  gItemAt = lensM . gItemAt @n @n

instance GHasFieldAt offset index s a => GHasFieldAt offset index (M1 D c s) a where
  gItemAt = lensM . gItemAt @offset @index

instance GHasFieldAt offset index s a => GHasFieldAt offset index (M1 C c s) a where
  gItemAt = lensM . gItemAt @offset @index

