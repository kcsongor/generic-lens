{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
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

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Internal.Constructors
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-name-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Internal.Constructors
  ( GAsConstructor (..)
  , GAsConstructor'

  , Context'
  , Context
  , Context_
  , Context0

  , derived0
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Errors
import Data.Generics.Product.Internal.HList

import GHC.Generics
import GHC.TypeLits (Symbol)
import Data.Kind (Constraint, Type)
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism

import GHC.TypeLits (TypeError, ErrorMessage (..))

derived0 :: forall ctor s t a b. Context0 ctor s t a b => Prism s t a b
derived0 = repIso . _GCtor @ctor
{-# INLINE derived0 #-}

type Context' ctor s a
  = ( Context0 ctor s s a a
    , ErrorUnless ctor s (HasCtorP ctor (Rep s)))

class Context (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b
instance
  ( ErrorUnless ctor s (HasCtorP ctor (Rep s))
  , GAsConstructor' ctor (Rep s) a -- TODO: add a test similar to #62 for prisms
  , GAsConstructor' ctor (Rep (Indexed s)) a'
  , GAsConstructor ctor (Rep s) (Rep t) a b
  , t ~ Infer s a' b
  , GAsConstructor' ctor (Rep (Indexed t)) b'
  , s ~ Infer t b' a
  ) => Context ctor s t a b

class Context_ (ctor :: Symbol) s t a b
instance
  ( ErrorUnless ctor s (HasCtorP ctor (Rep s))
  , GAsConstructor' ctor (Rep s) a -- TODO: add a test similar to #62 for prisms
  , GAsConstructor' ctor (Rep (Indexed s)) a'
  , GAsConstructor ctor (Rep s) (Rep t) a b
  , GAsConstructor' ctor (Rep (Indexed t)) b'
  , UnifyHead s t
  , UnifyHead t s
  ) => Context_ ctor s t a b

type Context0 ctor s t a b
  = ( Generic s
    , Generic t
    , GAsConstructor ctor (Rep s) (Rep t) a b
    , Defined (Rep s)
      (NoGeneric s '[ 'Text "arising from a generic prism focusing on the "
                      ':<>: QuoteType ctor ':<>: 'Text " constructor of type " ':<>: QuoteType a
                    , 'Text "in " ':<>: QuoteType s])
      (() :: Constraint)
    ) 

type family ErrorUnless (ctor :: Symbol) (s :: Type) (contains :: Bool) :: Constraint where
  ErrorUnless ctor s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a constructor named "
        ':<>: 'ShowType ctor
        )

  ErrorUnless _ _ 'True
    = ()
--------------------------------------------------------------------------------

-- |As 'AsConstructor' but over generic representations as defined by
--  "GHC.Generics".
class GAsConstructor (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b where
  _GCtor :: Prism (s x) (t x) a b

type GAsConstructor' ctor s a = GAsConstructor ctor s s a a

instance
  ( GIsList f g as bs
  , ListTuple a b as bs
  ) => GAsConstructor ctor (M1 C ('MetaCons ctor fixity fields) f) (M1 C ('MetaCons ctor fixity fields) g) a b where

  _GCtor = mIso . glist . tupled
  {-# INLINE _GCtor #-}

instance GSumAsConstructor ctor (HasCtorP ctor l) l r l' r' a b => GAsConstructor ctor (l :+: r) (l' :+: r') a b where
  _GCtor = _GSumCtor @ctor @(HasCtorP ctor l)
  {-# INLINE _GCtor #-}

instance GAsConstructor ctor f f' a b => GAsConstructor ctor (M1 D meta f) (M1 D meta f') a b where
  _GCtor = mIso . _GCtor @ctor
  {-# INLINE _GCtor #-}

class GSumAsConstructor (ctor :: Symbol) (contains :: Bool) l r l' r' a b | ctor l r -> a, ctor l' r' -> b where
  _GSumCtor :: Prism ((l :+: r) x) ((l' :+: r') x) a b

instance GAsConstructor ctor l l' a b => GSumAsConstructor ctor 'True l r l' r a b where
  _GSumCtor = left . _GCtor @ctor
  {-# INLINE _GSumCtor #-}

instance GAsConstructor ctor r r' a b => GSumAsConstructor ctor 'False l r l r' a b where
  _GSumCtor = right . _GCtor @ctor
  {-# INLINE _GSumCtor #-}
