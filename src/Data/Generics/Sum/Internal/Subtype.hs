{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Internal.Subtype
-- Copyright   :  (C) 2020 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Structural subtype relationships between sum types.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Internal.Subtype
  ( GAsSubtype (..)
  ) where

import Data.Generics.Product.Internal.HList
import Data.Generics.Sum.Internal.Typed

import Data.Kind
import GHC.Generics
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism
import Data.Generics.Internal.Families.Has

-- |As 'AsSubtype' but over generic representations as defined by
--  "GHC.Generics".
class GAsSubtype (subf :: Type -> Type) (supf :: Type -> Type) where
  _GSub :: Prism' (supf x) (subf x)

instance
  ( GSplash sub sup
  , GDowncast sub sup
  ) => GAsSubtype sub sup where
  _GSub f = prism _GSplash _GDowncast f
  {-# INLINE[0] _GSub #-}

--------------------------------------------------------------------------------

class GSplash (sub :: Type -> Type) (sup :: Type -> Type) where
  _GSplash :: sub x -> sup x

instance (GSplash a sup, GSplash b sup) => GSplash (a :+: b) sup where
  _GSplash (L1 rep) = _GSplash rep
  _GSplash (R1 rep) = _GSplash rep
  {-# INLINE[0] _GSplash #-}

instance
  ( GIsList subf subf as as
  , GAsType supf as
  ) => GSplash (C1 meta subf) supf where
  _GSplash p = build (_GTyped . fromIso (mIso . glist)) p
  {-# INLINE[0] _GSplash #-}

instance GSplash sub sup => GSplash (D1 c sub) sup where
  _GSplash (M1 m) = _GSplash m
  {-# INLINE[0] _GSplash #-}

--------------------------------------------------------------------------------

class GDowncast sub sup where
  _GDowncast :: sup x -> Either (sup x) (sub x)

instance
  ( GIsList sup sup as as
  , GDowncastC (HasPartialTypeP as sub) sub sup
  ) => GDowncast sub (C1 m sup) where
  _GDowncast (M1 m) = case _GDowncastC @(HasPartialTypeP as sub) m of
    Left _ -> Left (M1 m)
    Right r -> Right r
  {-# INLINE[0] _GDowncast #-}

instance (GDowncast sub l, GDowncast sub r) => GDowncast sub (l :+: r) where
  _GDowncast (L1 x) = case _GDowncast x of
    Left _ -> Left (L1 x)
    Right r -> Right r
  _GDowncast (R1 x) = case _GDowncast x of
    Left _ -> Left (R1 x)
    Right r -> Right r
  {-# INLINE[0] _GDowncast #-}

instance GDowncast sub sup => GDowncast sub (D1 m sup) where
  _GDowncast (M1 m) = case _GDowncast m of
    Left _ -> Left (M1 m)
    Right r -> Right r
  {-# INLINE[0] _GDowncast #-}

class GDowncastC (contains :: Bool) sub sup where
  _GDowncastC :: sup x -> Either (sup x) (sub x)

instance GDowncastC 'False sub sup where
  _GDowncastC sup = Left sup
  {-# INLINE[0] _GDowncastC #-}

instance
  ( GAsType sub subl
  , GIsList sup sup subl subl
  ) => GDowncastC 'True sub sup where
  _GDowncastC sup = Right (build (_GTyped . fromIso glist) sup)
  {-# INLINE[0] _GDowncastC #-}

