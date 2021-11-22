{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
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
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Text as T

import GHC.Generics
import Data.Generics.Internal.GenericN
import GHC.TypeLits
import Data.Generics.Internal.Errors

import "this" Data.Generics.Internal.VL.Traversal


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

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- HasTypes
--------------------------------------------------------------------------------

class HasTypes s a where
  types_ :: Traversal' s a

  types_ _ = pure
  {-# INLINE types_ #-}

instance
  ( HasTypesUsing ChGeneric s s a a
  ) => HasTypes s a where
  types_ = typesUsing_ @ChGeneric
  {-# INLINE types_ #-}

--------------------------------------------------------------------------------
data Void
instance {-# OVERLAPPING #-} HasTypes Void a where
  types_ _ = pure

instance {-# OVERLAPPING #-} HasTypes s Void where
  types_ _ = pure

instance {-# OVERLAPPING #-} HasTypesUsing ch Void Void a b where
  typesUsing_ _ = pure

instance {-# OVERLAPPING #-} HasTypesUsing ch s s Void Void where
  typesUsing_ _ = pure
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- HasTypesUsing
--------------------------------------------------------------------------------

-- | @since 1.2.0.0
class HasTypesUsing (ch :: Type) s t a b where
  typesUsing_ :: Traversal s t a b

instance {-# OVERLAPPABLE #-}
  ( HasTypesOpt ch (Interesting ch a s) s t a b
  ) => HasTypesUsing ch s t a b where
  typesUsing_ = typesOpt @ch @(Interesting ch a s)
  {-# INLINE typesUsing_ #-}

instance {-# OVERLAPPABLE #-} HasTypesUsing ch a b a b where
  typesUsing_ = id

-- | By adding instances to this class, we can override the default
-- behaviour in an ad-hoc manner.
-- For example:
--
-- @
-- instance HasTypesCustom Custom Opaque Opaque String String where
--   typesCustom f (Opaque str) = Opaque <$> f str
-- @
--
-- @since 1.2.0.0
class HasTypesCustom (ch :: Type) s t a b where
  -- | This function should never be used directly, only to override
  -- the default traversal behaviour. To actually use the custom
  -- traversal strategy, see 'typesUsing'. This is because 'typesUsing' does
  -- additional optimisations, like ensuring that nodes with no relevant members will
  -- not be traversed at runtime.
  typesCustom :: Traversal s t a b

instance {-# OVERLAPPABLE #-}
  ( GHasTypes ch (Rep s) (Rep t) a b
  , Generic s
  , Generic t
  -- if there's no Generic instance here, it means we got through the
  -- Children check by a user-defined custom strategy.
  -- Therefore, we can ignore the missing Generic instance, and
  -- instead report a missing HasTypesCustom instance
  , Defined (Rep s)
    (PrettyError '[ 'Text "No instance " ':<>: QuoteType (HasTypesCustom ch s t a b)])
    (() :: Constraint)
  ) => HasTypesCustom ch s t a b where
  typesCustom f s = to <$> gtypes_ @ch f (from s)

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------
-- TODO: these should never leak out in error messages

class HasTypesOpt (ch :: Type) (p :: Bool) s t a b where
  typesOpt :: Traversal s t a b

instance HasTypesCustom ch s t a b => HasTypesOpt ch 'True s t a b where
  typesOpt = typesCustom @ch

instance HasTypesOpt ch 'False s s a b where
  typesOpt _ = pure

--------------------------------------------------------------------------------
-- TODO: pull out recursion here.

class GHasTypes ch s t a b where
  gtypes_ :: Traversal (s x) (t x) a b

instance
  ( GHasTypes ch l l' a b
  , GHasTypes ch r r' a b
  ) => GHasTypes ch (l :*: r) (l' :*: r') a b where
  gtypes_ f (l :*: r) = (:*:) <$> gtypes_ @ch f l <*> gtypes_ @ch f r
  {-# INLINE gtypes_ #-}

instance
  ( GHasTypes ch l l' a b
  , GHasTypes ch r r' a b
  ) => GHasTypes ch (l :+: r) (l' :+: r') a b where
  gtypes_ f (L1 l) = L1 <$> gtypes_ @ch f l
  gtypes_ f (R1 r) = R1 <$> gtypes_ @ch f r
  {-# INLINE gtypes_ #-}

instance GHasTypes ch s t a b => GHasTypes ch (M1 m meta s) (M1 m meta t) a b where
  gtypes_ f (M1 s) = M1 <$> gtypes_ @ch f s
  {-# INLINE gtypes_ #-}

-- In the recursive case, we invoke 'HasTypesUsing' again, using the
-- same strategy
-- This instance is marked INCOHERENT, because
instance {-# INCOHERENT #-} HasTypesUsing ch s t a b => GHasTypes ch (Rec0 s) (Rec0 t) a b where
  gtypes_ f (K1 x) = K1 <$> typesUsing_ @ch f x
  {-# INLINE gtypes_ #-}

-- | The default instance for 'HasTypes' acts as a synonym for
-- 'HasTypesUsing ChGeneric', so in most cases this instance should
-- behave the same as the one above.
-- However, there might be overlapping instances defined for
-- 'HasTypes' directly, in which case we want to prefer those
-- instances (even though the custom instances should always be added to 'HasTypesCustom')
instance {-# OVERLAPPING #-} HasTypes b a => GHasTypes ChGeneric (Rec0 b) (Rec0 b) a a where
  gtypes_ f (K1 x) = K1 <$> types_ @b @a f x
  {-# INLINE gtypes_ #-}

instance GHasTypes ch U1 U1 a b where
  gtypes_ _ _ = pure U1
  {-# INLINE gtypes_ #-}

instance GHasTypes ch V1 V1 a b where
  gtypes_ _ = pure
  {-# INLINE gtypes_ #-}
