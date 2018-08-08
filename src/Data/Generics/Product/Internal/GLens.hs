{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.GLens
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive record field getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.GLens
  ( GLens (..)
  , GLens'
  , TyFun
  , Eval
  ) where

import Data.Generics.Internal.Profunctor.Lens (Lens, choosing, first, second)
import Data.Generics.Internal.Profunctor.Iso (kIso, sumIso, mIso)

import Data.Kind    (Type)
import GHC.Generics

type TyFun a b = a -> b -> Type
type family Eval (f :: TyFun a b) (x :: a) :: b

-- A generic lens that uses some predicate to determine which field to focus on
class GLens (pred :: TyFun (Type -> Type) Bool) (s :: Type -> Type) (t :: Type -> Type) a b | s pred -> a, t pred -> b where
  glens :: Lens (s x) (t x) a b

type GLens' pred s a = GLens pred s s a a

instance GProductLens (Eval pred l) pred l r l' r' a b
      => GLens pred (l :*: r) (l' :*: r') a b where

  glens = gproductField @(Eval pred l) @pred
  {-# INLINE glens #-}

instance (GLens pred l l' a b, GLens pred r r' a b) =>  GLens pred (l :+: r) (l' :+: r') a b where
  glens = sumIso . choosing (glens @pred) (glens @pred)
  {-# INLINE glens #-}

instance GLens pred (K1 r a) (K1 r b) a b where
  glens = kIso
  {-# INLINE glens #-}

instance (Functor g, GLens pred f g a b) => GLens pred (M1 m meta f) (M1 m meta g) a b where
  glens = mIso . glens @pred
  {-# INLINE glens #-}

class GProductLens (left :: Bool) (pred :: TyFun (Type -> Type) Bool) l r l' r' a b | pred l r -> a, pred l' r' -> b where
  gproductField :: Lens ((l :*: r) x) ((l' :*: r') x) a b

instance GLens pred l l' a b => GProductLens 'True pred l r l' r a b where
  gproductField = first . glens @pred
  {-# INLINE gproductField #-}

instance GLens pred r r' a b => GProductLens 'False pred l r l r' a b where
  gproductField = second . glens @pred
  {-# INLINE gproductField #-}
