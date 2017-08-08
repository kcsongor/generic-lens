{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Internal.HList
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Generics.Internal.HList
  ( (++)

  , ListTuple (..)

  , GCollectible (..)
  ) where

import Data.Kind (Type)
import GHC.Generics

data HList (xs :: [Type]) where
  Nil  :: HList '[]
  (:>) :: x -> HList xs -> HList (x ': xs)

infixr 5 :>

type family ((as :: [k]) ++ (bs :: [k])) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': as ++ bs

append :: HList as -> HList bs -> HList (as ++ bs)
append Nil       ys = ys
append (x :> xs) ys = x :> append xs ys

head' :: HList (a ': as) -> a
head' (x :> _) = x

--------------------------------------------------------------------------------
-- * Split HList

class Splittable (as :: [Type]) (bs :: [Type]) (cs :: [Type]) | as bs -> cs, as cs -> bs where
  split :: HList cs -> (HList as, HList bs)

instance Splittable '[] bs bs where
  split bs = (Nil, bs)

instance Splittable as bs cs => Splittable (a ': as) bs (a ': cs) where
  split (a :> as)
    = (a :> as', bs)
    where (as', bs) = split as

--------------------------------------------------------------------------------
-- * Convert tuples to/from HLists

class ListTuple (tuple :: Type) (as :: [Type]) | as -> tuple where
  type ListToTuple as :: Type
  tupleToList :: tuple -> HList as
  listToTuple :: HList as -> tuple

instance ListTuple () '[] where
  type ListToTuple '[] = ()
  tupleToList _ = Nil
  listToTuple _ = ()

instance ListTuple a '[a] where
  type ListToTuple '[a] = a
  tupleToList a
    = a :> Nil
  listToTuple (a :> Nil)
    = a

instance ListTuple (a, b) '[a, b] where
  type ListToTuple '[a, b] = (a, b)
  tupleToList (a, b)
    = a :> b :> Nil
  listToTuple (a :> b :> Nil)
    = (a, b)

instance ListTuple (a, b, c) '[a, b, c] where
  type ListToTuple '[a, b, c] = (a, b, c)
  tupleToList (a, b, c)
    = a :> b :> c :> Nil
  listToTuple (a :> b :> c :> Nil)
    = (a, b, c)

instance ListTuple (a, b, c, d) '[a, b, c, d] where
  type ListToTuple '[a, b, c, d] = (a, b, c, d)
  tupleToList (a, b, c, d)
    = a :> b :> c :> d:> Nil
  listToTuple (a :> b :> c :> d :> Nil)
    = (a, b, c, d)

instance ListTuple (a, b, c, d, e) '[a, b, c, d, e] where
  type ListToTuple '[a, b, c, d, e] = (a, b, c, d, e)
  tupleToList (a, b, c, d, e)
    = a :> b :> c :> d:> e :> Nil
  listToTuple (a :> b :> c :> d :> e :> Nil)
    = (a, b, c, d, e)

instance ListTuple (a, b, c, d, e, f) '[a, b, c, d, e, f] where
  type ListToTuple '[a, b, c, d, e, f] = (a, b, c, d, e, f)
  tupleToList (a, b, c, d, e, f)
    = a :> b :> c :> d:> e :> f :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> Nil)
    = (a, b, c, d, e, f)

instance ListTuple (a, b, c, d, e, f, g) '[a, b, c, d, e, f, g] where
  type ListToTuple '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  tupleToList (a, b, c, d, e, f, g)
    = a :> b :> c :> d:> e :> f :> g :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> Nil)
    = (a, b, c, d, e, f, g)

instance ListTuple (a, b, c, d, e, f, g, h) '[a, b, c, d, e, f, g, h] where
  type ListToTuple '[a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
  tupleToList (a, b, c, d, e, f, g, h)
    = a :> b :> c :> d:> e :> f :> g :> h :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> Nil)
    = (a, b, c, d, e, f, g, h)

instance ListTuple (a, b, c, d, e, f, g, h, j) '[a, b, c, d, e, f, g, h, j] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j] = (a, b, c, d, e, f, g, h, j)
  tupleToList (a, b, c, d, e, f, g, h, j)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> Nil)
    = (a, b, c, d, e, f, g, h, j)

instance ListTuple (a, b, c, d, e, f, g, h, j, k) '[a, b, c, d, e, f, g, h, j, k] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k] = (a, b, c, d, e, f, g, h, j, k)
  tupleToList (a, b, c, d, e, f, g, h, j, k)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> Nil)
    = (a, b, c, d, e, f, g, h, j, k)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l) '[a, b, c, d, e, f, g, h, j, k, l] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l] = (a, b, c, d, e, f, g, h, j, k, l)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l)

--------------------------------------------------------------------------------

class GCollectible (f :: Type -> Type) (as :: [Type]) | f -> as where
  type GCollect f :: [Type]
  gtoCollection   :: f x -> HList as
  gfromCollection :: HList as -> f x

instance
  ( GCollectible l as
  , GCollectible r bs
  , cs ~ (as ++ bs)
  , Splittable as bs cs
  ) => GCollectible (l :*: r) cs where
  type GCollect (l :*: r) = GCollect l ++ GCollect r

  gtoCollection (l :*: r)
    = gtoCollection l `append` gtoCollection r

  gfromCollection cs
    = gfromCollection as :*: gfromCollection bs
    where (as, bs) = split cs

instance GCollectible (Rec0 a) '[a] where
  type GCollect (Rec0 a) = '[a]
  gtoCollection   = (:> Nil) . unK1
  gfromCollection = K1 . head'

instance GCollectible U1 '[] where
  type GCollect U1    = '[]
  gtoCollection U1    = Nil
  gfromCollection Nil = U1

instance GCollectible f as => GCollectible (M1 m meta f) as where
  type GCollect (M1 m meta f) = GCollect f
  gtoCollection   = gtoCollection . unM1
  gfromCollection = M1 . gfromCollection
