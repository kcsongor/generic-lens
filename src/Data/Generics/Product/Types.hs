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

type Interesting f a = Snd (Interesting' (Rep f) a '[f])

type family Interesting' f (a :: Type) (seen :: [Type]) :: ([Type], Bool) where
  Interesting' (M1 _ m f) t seen
    = Interesting' f t seen
  -- The result of the left branch is passed on to the right branch in order to avoid duplicate work
  Interesting' (l :*: r) t seen
    = InterestingOr (Interesting' l t seen) r t
  Interesting' (l :+: r) t seen
    = InterestingOr (Interesting' l t seen) r t
  Interesting' (Rec0 t) t seen
    = '(seen, 'True)
  Interesting' (Rec0 Char)     _ seen = '(seen ,'False)
  Interesting' (Rec0 Double)   _ seen = '(seen ,'False)
  Interesting' (Rec0 Float)    _ seen = '(seen ,'False)
  Interesting' (Rec0 Integer)  _ seen = '(seen ,'False)
  Interesting' (Rec0 Int)      _ seen = '(seen ,'False)
  Interesting' (Rec0 Int8)     _ seen = '(seen ,'False)
  Interesting' (Rec0 Int16)    _ seen = '(seen ,'False)
  Interesting' (Rec0 Int32)    _ seen = '(seen ,'False)
  Interesting' (Rec0 Int64)    _ seen = '(seen ,'False)
  Interesting' (Rec0 Word)     _ seen = '(seen ,'False)
  Interesting' (Rec0 Word8)    _ seen = '(seen ,'False)
  Interesting' (Rec0 Word16)   _ seen = '(seen ,'False)
  Interesting' (Rec0 Word32)   _ seen = '(seen ,'False)
  Interesting' (Rec0 Word64)   _ seen = '(seen ,'False)
  Interesting' (Rec0 r) t seen
    = InterestingUnless (Elem r seen) (Rep r) t r seen
  Interesting' _ _ seen
    = '(seen, 'False)

-- Short circuit
-- Note: we only insert 'r' to the seen list if it's not already there (which is precisely when `s` is 'False)
type family InterestingUnless (s :: Bool) f (a :: Type) (r :: Type) (seen :: [Type]) :: ([Type], Bool) where
  InterestingUnless 'True _ _ _ seen = '(seen, 'False)
  InterestingUnless 'False f a r seen = Interesting' f a (r ': seen)

-- Short circuit
type family InterestingOr (b :: ([Type], Bool)) r t :: ([Type], Bool) where
  InterestingOr '(seen, 'True) _ _ = '(seen, 'True)
  InterestingOr '(seen, 'False) r t = Interesting' r t seen

type family Elem a as where
  Elem a (a ': _) = 'True
  Elem a (_ ': as) = Elem a as
  Elem a '[] = 'False

type family Snd a where
  Snd '(_, b) = b

