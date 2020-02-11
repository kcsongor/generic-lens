{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Internal.Typed
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-field-type-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Internal.Typed
  ( Context
  , derived
  , GAsType (..)
  ) where

import Data.Kind
import GHC.Generics

import GHC.TypeLits (TypeError, ErrorMessage (..), Symbol)
import Data.Generics.Internal.Errors
import Data.Generics.Internal.Families
import Data.Generics.Product.Internal.HList
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism

type Context a s
  = ( Generic s
    , ErrorUnlessOne a s (CollectPartialType (TupleToList a) (Rep s))
    , GAsType (Rep s) a
    , Defined (Rep s)
      (NoGeneric s '[ 'Text "arising from a generic prism focusing on a constructor of type " ':<>: QuoteType a])
      (() :: Constraint)
    )

derived :: Context a s => Prism' s a
derived = repIso . _GTyped
{-# INLINE derived #-}

type family ErrorUnlessOne (a :: Type) (s :: Type) (ctors :: [Symbol]) :: Constraint where
  ErrorUnlessOne _ _ '[_]
    = ()

  ErrorUnlessOne a s '[]
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a constructor whose field is of type "
        ':<>: 'ShowType a
        )

  ErrorUnlessOne a s cs
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " contains multiple constructors whose fields are of type "
        ':<>: 'ShowType a ':<>: 'Text "."
        ':$$: 'Text "The choice of constructor is thus ambiguous, could be any of:"
        ':$$: ShowSymbols cs
        )

-------------------------------------------------------------------------------- 

-- |As 'AsType' but over generic representations as defined by "GHC.Generics".
class GAsType (f :: Type -> Type) (as :: Type) where
  _GTyped :: Prism (f x) (f x) as as

instance
  ( GIsList f f as as
  , ListTuple a a as as
  ) => GAsType (M1 C meta f) a where
  _GTyped = mIso . glist . tupled
  {-# INLINE _GTyped #-}

instance GSumAsType (HasPartialTypeP (TupleToList a) l) l r a => GAsType (l :+: r) a where
  _GTyped = _GSumTyped @(HasPartialTypeP (TupleToList a) l)
  {-# INLINE _GTyped #-}

instance GAsType f a => GAsType (M1 D meta f) a where
  _GTyped = mIso . _GTyped
  {-# INLINE _GTyped #-}

class GSumAsType (contains :: Bool) l r (a :: Type) where
  _GSumTyped :: Prism ((l :+: r) x) ((l :+: r) x) a a

instance GAsType l a => GSumAsType 'True l r a where
  _GSumTyped = left . _GTyped
  {-# INLINE _GSumTyped #-}

instance GAsType r a => GSumAsType 'False l r a where
  _GSumTyped = right . _GTyped
  {-# INLINE _GSumTyped #-}
