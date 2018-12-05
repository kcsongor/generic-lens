{-# LANGUAGE DeriveGeneric #-}
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
{-# OPTIONS_GHC -Wno-unused-imports  #-}

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
    HasTypes
  , types

  , Interesting
  ) where

import Data.Kind
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)

import GHC.Generics
import Data.Generics.Internal.VL.Traversal
import Data.Generics.Internal.VL.Lens
import GHC.TypeLits
import Data.Generics.Internal.Errors

-- TODO [1.0.0.0]: use type-changing variant internally

-- |A lens that focuses on a field with a unique type in its parent type.
--  Compatible with the lens package's 'Control.Lens.Lens' type.
--
--  >>> over (types @Integer) (*2) [(Just 1, Just 2), (Just 3, Nothing)]
--  [(Just 2,Just 4),(Just 6,Nothing)]
--
--  
--  >>> data Foo = Foo deriving Show
--  >>> data Bad = Bad {field1 :: Foo, field2 :: Int} deriving (Generic, Show)
--  >>> over (types @Int) (+1) (Bad Foo 0)
--  ...
--  ...
--  ... No instance for Generic Boo
--  ...
--
--  >>> data Foo = Foo deriving Show
--  >>> over (types @Integer) (*2) [(Just 1, Foo)]
--  ...
--  ...
--  ... No instance for Generic Foo
--  ...
types :: forall a s. HasTypes s a => Traversal' s a
types = types_ @s @a
{-# INLINE types #-}

class HasTypes s a where
  types_ :: Traversal' s a

  default types_ :: Traversal' s a
  types_ _ = pure
  {-# INLINE types_ #-}

instance
  ( HasTypes' (Interesting s a) s a
  ) => HasTypes s a where
  types_ = types' @(Interesting s a)
  {-# INLINE types_ #-}

class HasTypes' (t :: Bool) s a where
  types' :: Traversal' s a

instance
  ( GHasTypes (Rep s) a
  , Generic s
  ) => HasTypes' 'True s a where
  types' f s = to <$> gtypes_ f (from s)
  --{-# INLINE types' #-}

instance HasTypes' 'False s a where
  types' _ = pure
  --{-# INLINE types' #-}

instance {-# OVERLAPPING #-} HasTypes Bool a
instance {-# OVERLAPPING #-} HasTypes Char a
instance {-# OVERLAPPING #-} HasTypes Double a
instance {-# OVERLAPPING #-} HasTypes Float a
instance {-# OVERLAPPING #-} HasTypes Integer a
instance {-# OVERLAPPING #-} HasTypes Ordering a
instance {-# OVERLAPPING #-} HasTypes Int a
instance {-# OVERLAPPING #-} HasTypes Int8 a
instance {-# OVERLAPPING #-} HasTypes Int16 a
instance {-# OVERLAPPING #-} HasTypes Int32 a
instance {-# OVERLAPPING #-} HasTypes Int64 a
instance {-# OVERLAPPING #-} HasTypes Word a
instance {-# OVERLAPPING #-} HasTypes Word8 a
instance {-# OVERLAPPING #-} HasTypes Word16 a
instance {-# OVERLAPPING #-} HasTypes Word32 a
instance {-# OVERLAPPING #-} HasTypes Word64 a

--------------------------------------------------------------------------------

class GHasTypes s a where
  gtypes_ :: Traversal' (s x) a

instance
  ( GHasTypes l a
  , GHasTypes r a
  ) => GHasTypes (l :*: r) a where
  gtypes_ f (l :*: r) = (:*:) <$> gtypes_ f l <*> gtypes_ f r
  {-# INLINE gtypes_ #-}

instance
  ( GHasTypes l a
  , GHasTypes r a
  ) => GHasTypes (l :+: r) a where
  gtypes_ f (L1 l) = L1 <$> gtypes_ f l
  gtypes_ f (R1 r) = R1 <$> gtypes_ f r
  {-# INLINE gtypes_ #-}

instance (GHasTypes s a) => GHasTypes (M1 m meta s) a where
  gtypes_ f (M1 s) = M1 <$> gtypes_ f s
  {-# INLINE gtypes_ #-}

instance {-# OVERLAPPING #-} GHasTypes (Rec0 a) a where
  gtypes_ f (K1 x) = K1 <$> f x
  {-# INLINE gtypes_ #-}

instance HasTypes b a => GHasTypes (Rec0 b) a where
  gtypes_ f (K1 x) = K1 <$> types_ @_ @a f x
  {-# INLINE gtypes_ #-}

instance GHasTypes U1 a where
  gtypes_ _ _ = pure U1
  {-# INLINE gtypes_ #-}

instance GHasTypes V1 a where
  gtypes_ _ = pure
  {-# INLINE gtypes_ #-}

type Interesting f a = Defined (Rep f) (NoGeneric f ('Text "")) (Snd (Interesting' f (Rep f) a '[f]))

type family Interesting' o f (a :: Type) (seen :: [Type]) :: ([Type], Bool) where
  Interesting' o (M1 _ m f) t seen
    = Interesting' o f t seen
  -- The result of the left branch is passed on to the right branch in order to avoid duplicate work
  Interesting' o (l :*: r) t seen
    = InterestingOr o (Interesting' o l t seen) r t
  Interesting' o (l :+: r) t seen
    = InterestingOr o (Interesting' o l t seen) r t
  Interesting' o (Rec0 t) t seen
    = '(seen, 'True)
  Interesting' _ (Rec0 Char)     _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Double)   _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Float)    _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Integer)  _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Int)      _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Int8)     _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Int16)    _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Int32)    _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Int64)    _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Word)     _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Word8)    _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Word16)   _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Word32)   _ seen = '(seen ,'False)
  Interesting' _ (Rec0 Word64)   _ seen = '(seen ,'False)
  Interesting' o (Rec0 r) t seen
    = Defined (Rep r) (NoGeneric r ('Text "arising from a traversal over " ':<>: 'ShowType o))
      (InterestingUnless o (Elem r seen) (Rep r) t r seen)
  Interesting' _ _ _ seen
    = '(seen, 'False)

-- Short circuit
-- Note: we only insert 'r' to the seen list if it's not already there (which is precisely when `s` is 'False)
type family InterestingUnless o (s :: Bool) f (a :: Type) (r :: Type) (seen :: [Type]) :: ([Type], Bool) where
  InterestingUnless o 'True _ _ _ seen = '(seen, 'False)
  InterestingUnless o 'False f a r seen = Interesting' o f a (r ': seen)

-- Short circuit
type family InterestingOr o (b :: ([Type], Bool)) r t :: ([Type], Bool) where
  InterestingOr o '(seen, 'True) _ _ = '(seen, 'True)
  InterestingOr o '(seen, 'False) r t = Interesting' o r t seen

type family Elem a as where
  Elem a (a ': _) = 'True
  Elem a (_ ': as) = Elem a as
  Elem a '[] = 'False

type family Snd a where
  Snd '(_, b) = b

type CanTraverseDeep f a = Defined (Rep f) (NoGeneric f ('Text "arising from a typed traveral.")) (CanTraverseDeep' f (Rep f) a '[f])

type family CanTraverseDeep' o f (a :: Type) (seen :: [Type]) :: [Type] where
  CanTraverseDeep' o (M1 _ m f) t seen
    = CanTraverseDeep' o f t seen
  -- The result of the left branch is passed on to the right branch in order to avoid duplicate work
  CanTraverseDeep' o (l :*: r) t seen
    = CanTraverseDeep' o r t (CanTraverseDeep' o l t seen)
  CanTraverseDeep' o (l :+: r) t seen
    = CanTraverseDeep' o r t (CanTraverseDeep' o l t seen)
  CanTraverseDeep' o (Rec0 t) t seen
    = seen
  CanTraverseDeep' _ (Rec0 Char)     _ seen = seen
  CanTraverseDeep' _ (Rec0 Double)   _ seen = seen
  CanTraverseDeep' _ (Rec0 Float)    _ seen = seen
  CanTraverseDeep' _ (Rec0 Integer)  _ seen = seen
  CanTraverseDeep' _ (Rec0 Int)      _ seen = seen
  CanTraverseDeep' _ (Rec0 Int8)     _ seen = seen
  CanTraverseDeep' _ (Rec0 Int16)    _ seen = seen
  CanTraverseDeep' _ (Rec0 Int32)    _ seen = seen
  CanTraverseDeep' _ (Rec0 Int64)    _ seen = seen
  CanTraverseDeep' _ (Rec0 Word)     _ seen = seen
  CanTraverseDeep' _ (Rec0 Word8)    _ seen = seen
  CanTraverseDeep' _ (Rec0 Word16)   _ seen = seen
  CanTraverseDeep' _ (Rec0 Word32)   _ seen = seen
  CanTraverseDeep' _ (Rec0 Word64)   _ seen = seen
  CanTraverseDeep' o (Rec0 r) t seen
    = Defined (Rep r) (NoGeneric r ('Text "arising from a traversal over " ':<>: 'ShowType o))
      (CanTraverseDeepUnless o (Elem r seen) (Rep r) t r seen)
  CanTraverseDeep' _ _ _ seen
    = seen

-- Short circuit
-- Note: we only insert 'r' to the seen list if it's not already there (which is precisely when `s` is 'False)
type family CanTraverseDeepUnless o (s :: Bool) f (a :: Type) (r :: Type) (seen :: [Type]) :: [Type] where
  CanTraverseDeepUnless o 'True _ _ _ seen = seen
  CanTraverseDeepUnless o 'False f a r seen = CanTraverseDeep' o f a (r ': seen)

data Foo = Foo

type family Force (xs :: [Type]) :: Constraint where
  Force '[] = ((), ())
  Force _ = ()

foo :: Force (CanTraverseDeep [(Int, Foo)] Int) => ()
foo = ()
