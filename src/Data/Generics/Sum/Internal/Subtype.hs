{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Internal.Subtype
-- Copyright   :  (C) 2017 Csongor Kiss
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

import Data.Generics.Product.Internal.List
import Data.Generics.Sum.Internal.Typed

import Data.Kind
import GHC.Generics
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism

-- |As 'AsSubtype' but over generic representations as defined by
--  "GHC.Generics".
class GAsSubtype (subf :: Type -> Type) (supf :: Type -> Type) where
  _GSub :: Prism' (supf x) (subf x)

instance
  ( GAsSubtype l supf
  , GAsSubtype r supf
  ) => GAsSubtype (l :+: r) supf where
  _GSub = without' _GSub _GSub . fromIso sumIso
  {-# INLINE _GSub #-}

instance
  ( GIsList () subf subf as as
  , GAsType supf as
  ) => GAsSubtype (C1 meta subf) supf where
  _GSub = _GTyped . fromIso (glist @()) . fromIso mIso
  {-# INLINE _GSub #-}

-- instance GAsType supf a => GAsSubtype (S1 meta (Rec0 a)) supf where
--   _GSub = _GTyped . fromIso (mIso . kIso)

instance GAsSubtype subf supf => GAsSubtype (D1 meta subf) supf where
  _GSub = _GSub . fromIso mIso
  {-# INLINE _GSub #-}
