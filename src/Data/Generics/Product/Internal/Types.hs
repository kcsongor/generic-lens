{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.Types
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Types
  ( GHasTypes (..)
  ) where

import Data.Kind    (Type)
import GHC.Generics

-- |As 'HasTypes' but over generic representations as defined by
--  "GHC.Generics".
class GHasTypes (f :: Type -> Type) a where
  gtypes :: forall g . Applicative g => (a -> g a) -> f a -> g (f a)

instance (GHasTypes l a, GHasTypes r a) => GHasTypes (l :*: r) a where
  gtypes f (l :*: r) = (:*:) <$> gtypes f l <*> gtypes f r
  {-# INLINE gtypes #-}

-- TODO:
-- instance (GHasTypes l a, GHasTypes r a) => GHasTypes (l :+: r) a where

instance GHasTypes (K1 R a) a where
  gtypes f (K1 x) = fmap K1 (f x)
  {-# INLINE gtypes #-}


instance {-# OVERLAPS #-} GHasTypes (K1 R a) b where
  gtypes _ k = pure k
  {-# INLINE gtypes #-}

instance GHasTypes f a => GHasTypes (M1 m meta f) a where
  gtypes f (M1 x) = M1 <$>  gtypes f x
  {-# INLINE gtypes #-}
