{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.Param
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive traversals of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.Param
  ( GHasParam (..)
  ) where

import GHC.Generics
import Data.Generics.Internal.VL.Iso
import Data.Generics.Internal.Families.Changing

class GHasParam (p :: Peano) s t a b | p t a -> s, p s b -> t where
  gparam :: forall g x.
    Applicative g => (a -> g b) -> s x -> g (t x)

instance (GHasParam p l l' a b, GHasParam p r r' a b) => GHasParam p (l :*: r) (l' :*: r') a b where
  gparam f (l :*: r) = (:*:) <$> gparam @p f l <*> gparam @p f r

instance (GHasParam p l l' a b, GHasParam p r r' a b) => GHasParam p (l :+: r) (l' :+: r') a b where
  gparam f (L1 l) = L1 <$> gparam @p f l
  gparam f (R1 r) = R1 <$> gparam @p f r

instance GHasParam p (K1 R (param p a 'PTag)) (K1 R (param p b 'PTag)) (param p a 'PTag) (param p b 'PTag) where
  gparam = kIso

instance {-# OVERLAPPABLE #-} (a ~ b) => GHasParam p (K1 R a) (K1 R b) c d where
  gparam _ = pure

instance GHasParam p s t a b => GHasParam p (M1 m meta s) (M1 m meta t) a b where
  gparam f (M1 x) = M1 <$> gparam @p f x

instance GHasParam p U1 U1 a b where
  gparam _ _ = pure U1

