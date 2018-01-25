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
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-field-type-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Internal.Typed
  ( GAsType (..)
  ) where

import Data.Kind
import GHC.Generics
import Data.Tagged

import Data.Generics.Internal.Families
import Data.Generics.Product.Internal.List
import Data.Generics.Internal.Profunctor.Iso
import Data.Generics.Internal.Profunctor.Prism

-- |As 'AsType' but over generic representations as defined by "GHC.Generics".
class GAsType (f :: Type -> Type) (as :: [((), Type)]) where
  _GTyped :: Prism (f x) (f x) (List as) (List as)
-- We create this specialised version as we use it in the subtype prism
-- If we don't create it, the opportunity for specialisation is only
-- created after specialisation happens, I think a late specialisation pass
-- would pick up this case.
{-# SPECIALISE _GTyped  :: (Tagged b b -> Tagged t t) #-}


instance
  ( GIsList () f f as as
  ) => GAsType (M1 C meta f) as where
  _GTyped = mIso . glist @()

instance GSumAsType (HasPartialTypeP a l) l r a => GAsType (l :+: r) a where
  _GTyped = _GSumTyped @(HasPartialTypeP a l)

instance GAsType f a => GAsType (M1 D meta f) a where
  _GTyped = mIso . _GTyped

class GSumAsType (contains :: Bool) l r (a :: [((), Type)]) where
  _GSumTyped :: Prism ((l :+: r) x) ((l :+: r) x) (List a) (List a)

instance GAsType l a => GSumAsType 'True l r a where
  _GSumTyped = left . _GTyped

instance GAsType r a => GSumAsType 'False l r a where
  _GSumTyped = right . _GTyped
