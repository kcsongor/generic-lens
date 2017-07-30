{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Generics.Product.Numbered
  ( HasPosition (..)

  , GHasPosition (..)
  ) where

import Data.Generics.Internal.Lens

import Data.Kind    (Constraint, Type)
import GHC.Generics
import GHC.TypeLits

class HasPosition (i :: Nat) a s | s i -> a where
  numbered :: Lens' s a

type BaseIndex
  = 1

instance (Generic s,
          hasP ~ HasPositionP BaseIndex i (Rep s),
          ErrorUnlessTrue i s hasP,
          hasP ~ 'True,
          GHasPosition BaseIndex i (Rep s) a)

      =>  HasPosition i a s where

  numbered = repIso . gnumbered @BaseIndex @i

type family ErrorUnlessTrue (i :: Nat) (s :: Type) (hasP :: Bool) :: Constraint where
  ErrorUnlessTrue i s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field at position "
        ':<>: 'ShowType i
        )

  ErrorUnlessTrue _ _ 'True
    = ()

class GHasPosition (offset :: Nat) (i :: Nat) (f :: Type -> Type) a | offset i f -> a where
  gnumbered :: Lens' (f x) a

instance GHasPosition i i (S1 meta (Rec0 a)) a where
  gnumbered = mIso . gnumbered @i @i

instance GHasPosition offset i (K1 R a) a where
  gnumbered f (K1 x) = fmap K1 (f x)

instance GHasPosition offset i f a => GHasPosition offset i (M1 D meta f) a where
  gnumbered = mIso . gnumbered @offset @i

instance GHasPosition offset i f a => GHasPosition offset i (M1 C meta f) a where
  gnumbered = mIso . gnumbered @offset @i

instance (dir ~ ChooseDirection offset i l r,
          offset' ~ Offset dir offset (SizeOf l),
          GProductHasPosition offset' i l r a dir)

      =>  GHasPosition offset i (l :*: r) a where

  gnumbered = gproductNumbered @offset' @i @_ @_ @_ @dir

class GProductHasPosition (offset :: Nat) (i :: Nat) l r a (dir :: Direction) | offset i l r dir -> a where
  gproductNumbered :: Lens' ((l :*: r) x) a

instance GHasPosition offset i l a => GProductHasPosition offset i l r a 'GoLeft where
  gproductNumbered = first . gnumbered @offset @i

instance GHasPosition offset i r a => GProductHasPosition offset i l r a 'GoRight where
  gproductNumbered = second . gnumbered @offset @i

data Direction
  = GoLeft
  | GoRight

type family ChooseDirection offset i l r :: Direction where
  ChooseDirection offset i l r
    = ChooseDirection' (HasPositionP offset i l) (HasPositionP (offset + SizeOf l) i r)

type family ChooseDirection' (lHas :: Bool) (rHas :: Bool) :: Direction where
  ChooseDirection' 'True _
    = 'GoLeft
  ChooseDirection' _ 'True
    = 'GoRight

type family HasPositionP (offset :: Nat) (i :: Nat) f :: Bool where
  HasPositionP offset i (D1 meta f)
    = HasPositionP offset i f
  HasPositionP n n (S1 _ _)
    = 'True
  HasPositionP _ _ (S1 _ _)
    = 'False
  HasPositionP offset i (l :*: r)
    = HasPositionP offset i l || HasPositionP (offset + SizeOf l) i r
  HasPositionP offset i (C1 meta f)
    = HasPositionP offset i f
  HasPositionP offset i (Rec0 _)
    = 'False
  HasPositionP offset i U1
    = 'False
  HasPositionP offset i V1
    = 'False
  HasPositionP offset i f
    = TypeError
        (     'Text "Cannot determine if the type "
        ':<>: 'ShowType f
        ':<>: 'Text " has position "
        ':<>: 'ShowType i
        ':<>: 'Text " starting from offset "
        ':<>: 'ShowType offset
        )

type family (x :: Bool) || (y :: Bool) :: Bool where
  'True || _ = 'True
  _     || y = y

type family SizeOf f :: Nat where
  SizeOf (l :*: r)
    = SizeOf l + SizeOf r
  SizeOf (D1 meta f)
    = SizeOf f
  SizeOf (C1 meta f)
    = SizeOf f
  SizeOf f
    = 1

type family Offset (dir :: Direction) (offset :: Nat) (sizeOfLeft :: Nat) :: Nat where
  Offset 'GoLeft  offset _  = offset
  Offset 'GoRight offset sl = offset + sl
