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

module Data.Generics.Product.Internal.Types where

import Data.Kind
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Text as T

import GHC.Generics
import Data.Generics.Internal.GenericN
import GHC.TypeLits
import Data.Generics.Internal.Errors


-- | The children of a type are the types of its fields.
-- The 'Children' type family maps a type @a@ to its set of children.
--
-- This type family is parameterized by a symbol @ch@ (that can be declared as
-- an empty data type).
-- The symbol 'ChGeneric' provides a default definition. You can create new
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
--   ChildrenCustom a = Children ChGeneric a
-- @
--
-- To use this definition, replace 'types' with @'typesUsing' \@Custom@.
type family Children (ch :: Type) (a :: Type) :: [Type]

-- | The default definition of 'Children'.
-- Primitive types from core libraries have no children, and other types are
-- assumed to be 'Generic'.
data ChGeneric
type instance Children ChGeneric a = ChildrenDefault a

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
  ChildrenDefault T.Text  = '[]
  ChildrenDefault (Param n _) = '[]
  ChildrenDefault a
   = Defined (Rep a)
    (NoGeneric a
      '[ 'Text "arising from a generic traversal."
       , 'Text "Either derive the instance, or define a custom traversal using HasTypesCustom"
       ])
    (ChildrenGeneric (Rep a) '[])

type family ChildrenGeneric (f :: k -> Type) (cs :: [Type]) :: [Type] where
  ChildrenGeneric (M1 _ _ f) cs = ChildrenGeneric f cs
  ChildrenGeneric (l :*: r) cs = ChildrenGeneric l (ChildrenGeneric r cs)
  ChildrenGeneric (l :+: r) cs = ChildrenGeneric l (ChildrenGeneric r cs)
  ChildrenGeneric (Rec0 a) cs = a ': cs
  ChildrenGeneric _ cs = cs

type Interesting (ch :: Type) (a :: Type) (t :: Type)
  = Defined_list (Children ch t) (NoChildren ch t)
    (IsNothing (Interesting' ch a '[t] (Children ch t)))

type family NoChildren (ch :: Type) (a :: Type) :: Constraint where
  NoChildren ch a = PrettyError
                    '[ 'Text "No type family instance for " ':<>: QuoteType (Children ch a)
                     , 'Text "arising from a traversal over " ':<>: QuoteType a
                     , 'Text "with custom strategy " ':<>: QuoteType ch
                     ]
                    

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
  InterestingUnless ch a seen t 'False
    = Defined_list (Children ch t) (NoChildren ch t)
      (Interesting' ch a (t ': seen) (Children ch t))

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
