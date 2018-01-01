{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Internal.List
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a product type and a flat HList.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Internal.List
  ( GIsList (..)
  ) where

import GHC.TypeLits
import Data.Generics.Internal.HList (type (++))
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics

data List (as :: [(Symbol, Type)]) where
  Nil :: List '[]
  (:>) :: a -> List as -> List ('(s, a) ': as)

infixr 5 :>

head' :: List ('(f, a) ': as) -> a
head' (x :> _) = x

class GIsList
  (rep :: * -> *)
  (rep' :: * -> *)
  (ts :: [(Symbol, Type)])
  (ts' :: [(Symbol, Type)]) | rep -> ts, rep' -> ts' where

  glist :: Lens (rep x) (rep' x) (List ts) (List ts')

instance GIsList
  (S1 ('MetaSel ('Just field) u s i) (Rec0 a))
  (S1 ('MetaSel ('Just field) u s i) (Rec0 b))
  '[ '(field, a)] '[ '(field, b)] where

  glist = mLens . kLens . (\f a -> head' <$> f (a :> Nil))

instance GIsList s t a b => GIsList (M1 m meta s) (M1 m meta t) a b where
  glist = mLens . glist

instance
  ( GIsList l l' a b
  , GIsList r r' a' b'
  , c ~ (a ++ a')
  , d ~ (b ++ b')
  ) => GIsList (l :*: r) (l' :*: r') c d where

  glist f (l :*: r) = _
