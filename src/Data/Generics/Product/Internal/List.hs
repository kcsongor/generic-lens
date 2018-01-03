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
{-# LANGUAGE TupleSections          #-}
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
  , IndexList (..)
  , List (..)
  ) where

import GHC.TypeLits
import Data.Generics.Internal.HList (type (++))
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics

data List (as :: [(Symbol, Type)]) where
  Nil :: List '[]
  (:>) :: a -> List as -> List ('(s, a) ': as)

class GIsList
  (f :: Type -> Type)
  (g :: Type -> Type)
  (as :: [(Symbol, Type)])
  (bs :: [(Symbol, Type)]) | f -> as, g -> bs, bs f -> g, as g -> f where

  glist :: Iso (f x) (g x) (List as) (List bs)

  glistL :: Lens (f x) (g x) (List as) (List bs)
  glistL f s = glist f s

  glistR :: Lens (List bs) (List as) (g x) (f x)
  glistR f s = fromIso glist f s

instance
  ( GIsList l l' as as'
  , GIsList r r' bs bs'
  , Appending List as bs cs as' bs' cs'
  , cs ~ (as ++ bs)
  , cs' ~ (as' ++ bs')
  ) => GIsList (l :*: r) (l' :*: r') cs cs' where

  glist = prodIso . pairing glist glist . appending

-- as ++ bs == cs
-- as' ++ bs' == cs'
class Appending f (as :: [k]) bs cs (as' :: [k]) bs' cs' | as bs cs cs' -> as' bs', as' bs' cs cs' -> as bs, as bs -> cs, as' bs' -> cs' where
  appending :: Iso (f as, f bs) (f as', f bs') (f cs) (f cs')

instance Appending List '[] bs bs '[] bs' bs' where
  appending = iso snd (Nil,)

instance Appending List xs bs cs xs' bs' cs'
  => Appending List ('(f, x) ': xs) bs ('(f, x) ': cs) ('(f, x') ': xs') bs' ('(f, x') ': cs') where
  appending = pairing (fromIso consing) id . assoc3 . pairing id appending . consing

instance {-# OVERLAPS #-}
  GIsList (S1 ('MetaSel ('Just field) u s i) (Rec0 a))
          (S1 ('MetaSel ('Just field) u s i) (Rec0 b))
          '[ '(field, a)] '[ '(field, b)] where
  glist = mIso . kIso . singleton

instance GIsList U1 U1 '[] '[] where
  glist = iso (const Nil) (const U1)

instance GIsList f g as bs => GIsList (M1 m meta f) (M1 m meta g) as bs where
  glist = mIso . glist

singleton :: Iso a b (List '[ '(field, a)]) (List '[ '(field, b)])
singleton = iso (:> Nil) (\(x :> _) -> x)

consing :: Iso (a, List as) (b, List bs) (List ('(f, a) ': as)) (List ('(f, b) ': bs))
consing = iso (\(x, xs) -> x :> xs) (\(x :> xs) -> (x, xs))

--------------------------------------------------------------------------------
class IndexList (i :: Nat) as bs a b | i as -> a, i bs -> b, i as b -> bs, i bs a -> as where
  point :: Lens (List as) (List bs) a b

instance {-# OVERLAPPING #-}
  ( as ~ ('(f, a) ': as')
  , bs ~ ('(f, b) ': as')
  ) => IndexList 0 as bs a b where
  point f (x :> xs) = (:> xs) <$> f x

instance
  ( IndexList (n - 1) as' bs' a b
  , as ~ (x ': as')
  , bs ~ (x ': bs')
  ) => IndexList n as bs a b where
  point f (x :> xs) = (x :>) <$> point @(n - 1) f xs
