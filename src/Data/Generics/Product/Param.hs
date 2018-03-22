{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Product.Param
-- Copyright   : (C) 2018 Csongor Kiss
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- Derive traversal over type parameters
--
--------------------------------------------------------------------------------

module Data.Generics.Product.Param
  ( Rec (Rec) -- TODO: this has to be re-exported so the constructor is visible for Coercible... is there a better way?
  , HasParam (..)
  ) where

import GHC.TypeLits
import Data.Generics.Internal.Void
import Data.Generics.Internal.Families.Changing
import Data.Generics.Internal.VL.Traversal

import GHC.Generics
import Data.Kind
import Data.Generics.Internal.VL.Iso
import Data.Generics.Internal.GenericN

class HasParam (p :: Nat) s t a b | p t a -> s, p s b -> t, p s -> a, p t -> b where
  param :: Applicative g => (a -> g b) -> s -> g t

instance
  ( GenericN s
  , GenericN t
  -- TODO: merge the old 'Changing' code with 'GenericN'
  , s ~ Infer t (P n b 'PTag) a
  , t ~ Infer s (P n a 'PTag) b
  , Error ((ArgCount s) <=? n) n (ArgCount s) s
  , a ~ ArgAt s n
  , b ~ ArgAt t n
  , GHasParam n (RepN s) (RepN t) a b
  ) => HasParam n s t a b where

  param = confusing (\f s -> toN <$> gparam @n f (fromN s))
  {-# INLINE param #-}

type family Error (b :: Bool) (expected :: Nat) (actual :: Nat) (s :: Type) :: Constraint where
  Error 'False _ _ _
    = ()

  Error 'True expected actual typ
    = TypeError
        (     'Text "Expected a type with at least "
        ':<>: 'ShowType (expected + 1)
        ':<>: 'Text " parameters, but "
        ':$$: 'ShowType typ
        ':<>: 'Text " only has "
        ':<>: 'ShowType actual
        )

-- TODO [1.0.0.0]: none of this is needed.

instance {-# OVERLAPPING #-} HasParam p (Void1 a) (Void1 b) a b where
  param = undefined

class GHasParam (p :: Nat) s t a b where
  gparam :: forall g (x :: Type).  Applicative g => (a -> g b) -> s x -> g (t x)

instance (GHasParam p l l' a b, GHasParam p r r' a b) => GHasParam p (l :*: r) (l' :*: r') a b where
  gparam f (l :*: r) = (:*:) <$> gparam @p f l <*> gparam @p f r

instance (GHasParam p l l' a b, GHasParam p r r' a b) => GHasParam p (l :+: r) (l' :+: r') a b where
  gparam f (L1 l) = L1 <$> gparam @p f l
  gparam f (R1 r) = R1 <$> gparam @p f r

instance GHasParam p U1 U1 a b where
  gparam _ _ = pure U1

instance GHasParam p s t a b => GHasParam p (M1 m meta s) (M1 m meta t) a b where
  gparam f (M1 x) = M1 <$> gparam @p f x

-- the parameter we're looking for
instance GHasParam p (Rec (param p) a) (Rec (param p) b) a b where
  gparam = recIso

-- other recursion
instance {-# OVERLAPPABLE #-}
  ( GHasParamRec (LookupParam si p) s t a b
  -- TODO: reindex `ti`
  ) => GHasParam p (Rec si s) (Rec ti t) a b where
  gparam f (Rec (K1 x)) = Rec . K1 <$> gparamRec @(LookupParam si p) f x

class GHasParamRec (param :: Maybe Nat) s t a b | param t a b -> s, param s a b -> t where
  gparamRec :: forall g.  Applicative g => (a -> g b) -> s -> g t

instance GHasParamRec 'Nothing a a c d where
  gparamRec _ = pure

instance (HasParam n s t a b) => GHasParamRec ('Just n) s t a b where
  gparamRec = param @n
