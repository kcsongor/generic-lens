{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Internal.GenericDeep
-- Copyright   : (C) 2018 Csongor Kiss
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- Deep generic representation
--
--------------------------------------------------------------------------------

module Data.Generics.Internal.GenericDeep where

import Data.Kind
import GHC.Generics
import Data.Coerce

class
  ( Coercible (Rep a) (Deep a)
  , Generic a
  ) => GenericDeep (a :: Type) where
  type family Deep (a :: Type) :: Type -> Type
  type instance Deep t = Co (Simpl (Rep t)) '[t]
  toDeep :: Deep a x -> a
  fromDeep :: a -> Deep a x

instance
  ( Coercible (Rep a) (Deep a)
  , Generic a
  ) => GenericDeep a where
  toDeep :: forall x. Deep a x -> a
  toDeep   = coerce (to :: Rep a x -> a)
  {-# INLINE[0] toDeep #-}

  fromDeep :: forall x. a -> Deep a x
  fromDeep = coerce (from :: a -> Rep a x)
  {-# INLINE[0] fromDeep #-}

newtype Node a (b :: * -> *) x = Node (K1 R a x)
newtype Primitive a x          = Primitive (Rec0 a x)
newtype Found a x              = Found (Rec0 a x)
newtype Recurse a x            = Recurse (Rec0 a x)
newtype FoundAndRecurse a x    = FoundAndRecurse (Rec0 a x)
newtype SStop a x              = SStop (Rec0 a x)

type family Simpl (f :: * -> *) :: * -> * where
  Simpl (M1 _ _ s)
    = Simpl s
  Simpl (l :+: r)
    = Simpl l :+: Simpl r
  Simpl (l :*: r)
    = Simpl l :*: Simpl r
  Simpl a
    = a

type family Elem x xs where
  Elem _ '[] = 'False
  Elem x (x ': _) = 'True
  Elem x (_ ': xs) = Elem x xs

type family CIf b t r s where
  CIf 'True t _ _ = t
  CIf 'False _ r s = Node r (Co (Simpl (Rep r)) s)

type family Co rep (seen :: [*]) where
  Co (l :*: r) s
    = (:*:) (Co l s) (Co r s)
  Co (l :+: r) s
    = (:+:) (Co l s) (Co r s)
  Co (Rec0 Bool)     s = Primitive Bool
  Co (Rec0 Char)     s = Primitive Char
  Co (Rec0 Double)   s = Primitive Double
  Co (Rec0 Float)    s = Primitive Float
  Co (Rec0 Int)      s = Primitive Int
  Co (Rec0 Integer)  s = Primitive Integer
  Co (Rec0 Ordering) s = Primitive Ordering
  Co (Rec0 r) s
    = CIf (Elem r s) (Rec0 r) r (r ': s)
  Co U1 s
    = U1
