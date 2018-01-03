{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeInType             #-}
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

data List (as :: [(m, Type)]) where
  Nil :: List '[]
  (:>) :: a -> List as -> List ('(s, a) ': as)

class GIsList
  (m :: Type)
  (f :: Type -> Type)
  (g :: Type -> Type)
  (as :: [(m, Type)])
  (bs :: [(m, Type)]) | m f -> as, m g -> bs, bs f -> g, as g -> f where

  glist :: Iso (f x) (g x) (List as) (List bs)

  glistL :: Lens (f x) (g x) (List as) (List bs)
  glistL f s = glist @m f s

  glistR :: Lens (List bs) (List as) (g x) (f x)
  glistR f s = fromIso (glist @m) f s

instance
  ( GIsList m l l' as as'
  , GIsList m r r' bs bs'
  , Appending List as bs cs as' bs' cs'
  , cs ~ (as ++ bs)
  , cs' ~ (as' ++ bs')
  ) => GIsList m (l :*: r) (l' :*: r') cs cs' where

  glist = prodIso . pairing (glist @m) (glist @m) . appending

-- as ++ bs == cs
-- as' ++ bs' == cs'
class Appending f (as :: [k]) bs cs (as' :: [k]) bs' cs' | as bs cs cs' -> as' bs', as' bs' cs cs' -> as bs, as bs -> cs, as' bs' -> cs' where
  appending :: Iso (f as, f bs) (f as', f bs') (f cs) (f cs')

instance Appending List '[] bs bs '[] bs' bs' where
  appending = iso snd (Nil,)

instance Appending List as bs cs as' bs' cs'
  => Appending List ('(f, a) ': as) bs ('(f, a) ': cs) ('(f, a') ': as') bs' ('(f, a') ': cs') where
  appending = pairing (fromIso consing) id . assoc3 . pairing id appending . consing

instance {-# OVERLAPS #-}
  GIsList Symbol
          (S1 ('MetaSel ('Just field) u s i) (Rec0 a))
          (S1 ('MetaSel ('Just field) u s i) (Rec0 b))
          '[ '(field, a)] '[ '(field, b)] where
  glist = mIso . kIso . singleton

instance GIsList () (Rec0 a) (Rec0 b) '[ '( '(), a)] '[ '( '(), b)] where
  glist = kIso . singleton

instance GIsList m U1 U1 '[] '[] where
  glist = iso (const Nil) (const U1)

instance GIsList m f g as bs => GIsList m (M1 t meta f) (M1 t meta g) as bs where
  glist = mIso . glist @m

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
  {-# INLINE point #-}

instance
  ( IndexList (n - 1) as' bs' a b
  , as ~ (x ': as')
  , bs ~ (x ': bs')
  ) => IndexList n as bs a b where
  point f (x :> xs) = (x :>) <$> point @(n - 1) f xs
  {-# INLINE point #-}
