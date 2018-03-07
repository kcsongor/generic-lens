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
  type instance Deep t = Fst (Co (Rep t) '[t])
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

type family Elem x xs where
  Elem x (x ': _) = 'True
  Elem x (_ ': xs) = Elem x xs
  Elem _ '[] = 'False
--  Elem x (x ': _ ': _ ': _ ': _) = 'True
--  Elem x (_ ': x ': _ ': _ ': _) = 'True
--  Elem x (_ ': _ ': x ': _ ': _) = 'True
--  Elem x (_ ': _ ': _ ': x ': _) = 'True
--  Elem x (_ ': _ ': _ ': _ ': xs) = Elem x xs
--  Elem _ _ = 'False

type family CIf b t r s where
  CIf 'True t _ s = '(t, s)
  CIf 'False _ r s = Put Node r (Co (Rep r) s)

type family CombineWith f l r where
  CombineWith f '(l, s) r = Put f l (Co r s)

type family Put f l r where
  Put f l '(r, s) = '(f l r, s)

type family Fst a where
  Fst '(a, _) = a

type family Co rep (seen :: [*]) :: (* -> *, [*]) where
  Co (M1 _ _ f) s
    = Co f s
  Co (l :*: r) s
    = CombineWith (:*:) (Co l s) r
  Co (l :+: r) s
    = CombineWith (:+:) (Co l s) r
  Co (Rec0 Bool)     s = '(Primitive Bool, s)
  Co (Rec0 Char)     s = '(Primitive Char, s)
  Co (Rec0 Double)   s = '(Primitive Double, s)
  Co (Rec0 Float)    s = '(Primitive Float, s)
  Co (Rec0 Int)      s = '(Primitive Int, s)
  Co (Rec0 Integer)  s = '(Primitive Integer, s)
  Co (Rec0 Ordering) s = '(Primitive Ordering, s)
  Co (Rec0 r) s
    = CIf (Elem r s) (Rec0 r) r (r ': s)
  Co U1 s
    = '(U1, s)
