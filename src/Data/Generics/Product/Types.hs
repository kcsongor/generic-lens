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

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Types
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Types where

  --( -- *Traversals
  --  --
  --  --  $example
  --  HasTypes (..)
  --) where

import Data.Kind
import GHC.TypeLits
import qualified GHC.Generics as G

import Data.Generics.Internal.Simple
import Data.Generics.Internal.VL.Traversal

class HasTypes_ s t a b where
  types_ :: Traversal s t a b

instance {-# OVERLAPPING #-} t ~ Char => HasTypes_ Char t a b where
  types_ _ = pure
  {-# INLINE types_ #-}
instance {-# OVERLAPPING #-} t ~ Double => HasTypes_ Double t a b where
  types_ _ = pure
  {-# INLINE types_ #-}
instance {-# OVERLAPPING #-} t ~ Float => HasTypes_ Float t a b where
  types_ _ = pure
  {-# INLINE types_ #-}
instance {-# OVERLAPPING #-} t ~ Int => HasTypes_ Int t a b where
  types_ _ = pure
  {-# INLINE types_ #-}
instance {-# OVERLAPPING #-} t ~ Integer => HasTypes_ Integer t a b where
  types_ _ = pure
  {-# INLINE types_ #-}
instance {-# OVERLAPPING #-} t ~ Word => HasTypes_ Word t a b where
  types_ _ = pure
  {-# INLINE types_ #-}

type HasTypes s a = HasTypes_ s s a a
types :: forall a s. HasTypes s a => Traversal s s a a
types f s = types_ f s
{-# INLINE types #-}

class HasTypesOpt (i :: Bool) s t a b where
  typesOpt :: Traversal s t a b

instance (Generic s, Generic t , GHasTypes (Rep s) (Rep t) a b)
    => HasTypesOpt 'True s t a b where
  typesOpt = repIso . gtypes

instance HasTypesOpt 'False s s a b where
  typesOpt _ = pure

instance (Generic s, Generic t , HasTypesOpt (Interesting s a) s t a b)
    => HasTypes_ s t a b where
  types_ = typesOpt @(Interesting s a)
  {-# INLINE types_ #-}

--------------------------------------------------------------------------------

class GHasTypes s t a b where
  gtypes :: Traversal s t a b

instance GHasTypes s t a b => GHasTypes (M1 m s) (M1 m t) a b where
  gtypes = mIso . gtypes
  {-# INLINE gtypes #-}

instance (GHasTypes l_1 l_2 a b , GHasTypes r_1 r_2 a b)
    => GHasTypes (l_1 :+: r_1) (l_2 :+: r_2) a b where
  gtypes f (L1 l) = L1 <$> gtypes f l
  gtypes f (R1 r) = R1 <$> gtypes f r
  {-# INLINE gtypes #-}

instance (GHasTypes l_1 l_2 a b , GHasTypes r_1 r_2 a b)
    => GHasTypes (l_1 :*: r_1) (l_2 :*: r_2) a b where
  gtypes f (l :*: r) = (:*:) <$> gtypes f l <*> gtypes f r
  {-# INLINE gtypes #-}

instance {-# OVERLAPPING #-} GHasTypes (Rec0 a) (Rec0 b) a b where
  gtypes = kIso
  {-# INLINE gtypes #-}

instance HasTypes_ s t a b => GHasTypes (Rec0 s) (Rec0 t) a b where
  gtypes = kIso . types_
  {-# INLINE gtypes #-}

instance GHasTypes U1 U1 a b where
  gtypes _ = pure
  {-# INLINE gtypes #-}

instance GHasTypes V1 V1 a b where
  gtypes _ = pure
  {-# INLINE gtypes #-}

newtype Param (i :: Nat) a = Param { unParam :: a }
  deriving G.Generic

--------------------------------------------------------------------------------
type Interesting f a = Snd (Interesting' (Rep f) a '[f])

type family Interesting' (f :: Type) (a :: Type) (seen :: [Type]) :: ([Type], Bool) where
  Interesting' (M1 m f) t seen
    = Interesting' f t seen
  -- The result of the left branch is passed on to the right branch in order to avoid duplicate work
  Interesting' (l :*: r) t seen
    = InterestingOr (Interesting' l t seen) r t
  Interesting' (l :+: r) t seen
    = InterestingOr (Interesting' l t seen) r t
--  Interesting' (Rec0 (Param _ _)) _ seen
--    = '(seen, 'False)
  Interesting' (Rec0 t) t seen
    = '(seen, 'True)
  Interesting' (Rec0 Char)     _ seen = '(seen ,'False)
  Interesting' (Rec0 Double)   _ seen = '(seen ,'False)
  Interesting' (Rec0 Float)    _ seen = '(seen ,'False)
  Interesting' (Rec0 Int)      _ seen = '(seen ,'False)
  Interesting' (Rec0 Integer)  _ seen = '(seen ,'False)
  Interesting' (Rec0 r) t seen
    = InterestingUnless (Elem r seen) (Rep r) t r seen
  Interesting' V1 _ seen
    = '(seen, 'False)
  Interesting' U1 _ seen
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

