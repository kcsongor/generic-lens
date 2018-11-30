{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Types
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Types
  ( -- *Traversals
    --
    --  $example
    HasTypes
  , types

  , HasTypesUsing
  , typesUsing

  , HasTypes'
  , types'

  , Children
  , ChDefault
  ) where

import Data.Kind
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)

import GHC.Generics
import Data.Generics.Internal.VL.Traversal

-- TODO [1.0.0.0]: use type-changing variant internally

types :: forall a s. HasTypes s a => Traversal' s a
types = types_ @s @a
{-# INLINE types #-}

class HasTypes s a where
  types_ :: Traversal' s a

  default types_ :: Traversal' s a
  types_ _ = pure
  {-# INLINE types_ #-}

instance
  ( HasTypesUsing ChDefault s a
  ) => HasTypes s a where
  types_ = typesUsing @ChDefault
  {-# INLINE types_ #-}

class HasTypesUsing (ch :: Type) s a where
  typesUsing :: Traversal' s a

instance
  ( HasTypes1 ch (Interesting ch a s) s a
  ) => HasTypesUsing ch s a where
  typesUsing = types1 @ch @(Interesting ch a s)
  {-# INLINE typesUsing #-}

class HasTypes0 (ch :: Type) s a where
  types0 :: Traversal' s a

instance {-# OVERLAPPING #-} HasTypes0 ch a a where
  types0 = id

instance
  ( HasTypes1 ch (Interesting ch a s) s a
  ) => HasTypes0 ch s a where
  types0 = types1 @ch @(Interesting ch a s)

class HasTypes1 (ch :: Type) (t :: Bool) s a where
  types1 :: Traversal' s a

instance HasTypes' ch s a => HasTypes1 ch 'True s a where
  types1 = types' @ch

instance HasTypes1 ch 'False s a where
  types1 _ = pure
  --{-# INLINE types1 #-}

class HasTypes' (ch :: Type) s a where
  types' :: Traversal' s a

instance
  ( GHasTypes ch (Rep s) a
  , Generic s
  ) => HasTypes' ch s a where
  types' f s = to <$> gtypes_ @ch f (from s)
  --{-# INLINE types' #-}

--------------------------------------------------------------------------------

class GHasTypes ch s a where
  gtypes_ :: Traversal' (s x) a

instance
  ( GHasTypes ch l a
  , GHasTypes ch r a
  ) => GHasTypes ch (l :*: r) a where
  gtypes_ f (l :*: r) = (:*:) <$> gtypes_ @ch f l <*> gtypes_ @ch f r
  {-# INLINE gtypes_ #-}

instance
  ( GHasTypes ch l a
  , GHasTypes ch r a
  ) => GHasTypes ch (l :+: r) a where
  gtypes_ f (L1 l) = L1 <$> gtypes_ @ch f l
  gtypes_ f (R1 r) = R1 <$> gtypes_ @ch f r
  {-# INLINE gtypes_ #-}

instance (GHasTypes ch s a) => GHasTypes ch (M1 m meta s) a where
  gtypes_ f (M1 s) = M1 <$> gtypes_ @ch f s
  {-# INLINE gtypes_ #-}

instance HasTypes0 ch b a => GHasTypes ch (Rec0 b) a where
  gtypes_ f (K1 x) = K1 <$> types0 @ch @b @a f x
  {-# INLINE gtypes_ #-}

instance GHasTypes ch U1 a where
  gtypes_ _ _ = pure U1
  {-# INLINE gtypes_ #-}

instance GHasTypes ch V1 a where
  gtypes_ _ = pure
  {-# INLINE gtypes_ #-}

type family ChildrenGeneric (f :: k -> Type) (cs :: [Type]) :: [Type] where
  ChildrenGeneric (M1 _ _ f) cs = ChildrenGeneric f cs
  ChildrenGeneric (l :*: r) cs = ChildrenGeneric l (ChildrenGeneric r cs)
  ChildrenGeneric (l :+: r) cs = ChildrenGeneric l (ChildrenGeneric r cs)
  ChildrenGeneric (Rec0 a) cs = a ': cs
  ChildrenGeneric _ cs = cs

-- | The children of a type are the types of its fields.
-- The 'Children' type family maps a type @a@ to its set of children.
--
-- This type family is parameterized by a symbol @ch@ (that can be declared as
-- an empty data type).
-- The symbol 'ChDefault' provides a default definition. You can create new
-- symbols to override the set of children of abstract, non-generic types.
--
-- The following example declares a @Custom@ symbol to redefine 'Children'
-- for some abstract types from the @time@ library.
--
-- @
-- data Custom
-- type instance 'Children' Custom a = ChildrenCustom a
--
-- type family ChildrenCustom (a :: Type) where
--   ChildrenCustom DiffTime        = '[]
--   ChildrenCustom NominalDiffTime = '[]
--   -- Add more custom mappings here.
--
--   ChildrenCustom a = Children ChDefault a
-- @
--
-- To use this definition, replace 'types' with @'typesUsing' \@Custom@.
type family Children (ch :: Type) (a :: Type) :: [Type]

-- | The default definition of 'Children'.
-- Primitive types from core libraries have no children, and other types are
-- assumed to be 'Generic'.
data ChDefault
type instance Children ChDefault a = ChildrenDefault a

type family ChildrenDefault (a :: Type) :: [Type] where
  ChildrenDefault Char    = '[]
  ChildrenDefault Double  = '[]
  ChildrenDefault Float   = '[]
  ChildrenDefault Integer = '[]
  ChildrenDefault Int     = '[]
  ChildrenDefault Int8    = '[]
  ChildrenDefault Int16   = '[]
  ChildrenDefault Int32   = '[]
  ChildrenDefault Int64   = '[]
  ChildrenDefault Word    = '[]
  ChildrenDefault Word8   = '[]
  ChildrenDefault Word16  = '[]
  ChildrenDefault Word32  = '[]
  ChildrenDefault Word64  = '[]
  ChildrenDefault a       = ChildrenGeneric (Rep a) '[]

type Interesting (ch :: Type) (a :: Type) (t :: Type)
  = IsNothing (Interesting' ch a '[t] (Children ch t))

type family Interesting' (ch :: Type) (a :: Type) (seen :: [Type]) (ts :: [Type]) :: Maybe [Type] where
  Interesting' ch _ seen '[] = 'Just seen
  Interesting' ch a seen (t ': ts) =
    InterestingOr ch a (InterestingUnless ch a seen t (Elem t seen)) ts

-- Short circuit
-- Note: we only insert 't' to the seen list if it's not already there (which is precisely when `s` is 'False)
type family InterestingUnless
    (ch :: Type) (a :: Type) (seen :: [Type]) (t :: Type) (alreadySeen :: Bool) ::
    Maybe [Type] where
  InterestingUnless ch a seen a _ = 'Nothing
  InterestingUnless ch a seen t 'True = 'Just seen
  InterestingUnless ch a seen t 'False = Interesting' ch a (t ': seen) (Children ch t)

-- Short circuit
type family InterestingOr
    (ch :: Type) (a :: Type) (seen' :: Maybe [Type]) (ts :: [Type]) ::
    Maybe [Type] where
  InterestingOr ch a 'Nothing _ = 'Nothing
  InterestingOr ch a ('Just seen) ts = Interesting' ch a seen ts

type family Elem a as where
  Elem a (a ': _) = 'True
  Elem a (_ ': as) = Elem a as
  Elem a '[] = 'False

type family IsNothing a where
  IsNothing ('Just _) = 'False
  IsNothing 'Nothing = 'True

