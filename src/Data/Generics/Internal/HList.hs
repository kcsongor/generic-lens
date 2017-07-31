{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Generics.Internal.HList
  ( HList (..)

  , append
  , (++)
  , Splittable (..)

  , ListTuple (..)

  , GCollectible (..)
  ) where

import Data.Kind
import GHC.Generics

data HList (xs :: [Type]) where
  Nil  :: HList '[]
  (:>) :: x -> HList xs -> HList (x ': xs)

infixr 5 :>

append :: HList as -> HList bs -> HList (as ++ bs)
append Nil       ys = ys
append (x :> xs) ys = x :> append xs ys

type family ((as :: [k]) ++ (bs :: [k])) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': as ++ bs

class Splittable (as :: [Type]) (bs :: [Type]) (cs :: [Type]) | as bs -> cs, as cs -> bs where
  split :: HList cs -> (HList as, HList bs)

instance Splittable '[] bs bs where
  split bs = (Nil, bs)

instance Splittable as bs cs => Splittable (a ': as) bs (a ': cs) where
  split (a :> as) = let (as', bs) = split as in (a :> as', bs)

class ListTuple (tuple :: Type) (as :: [Type]) | as -> tuple where
  tupleToList :: tuple -> HList as
  listToTuple :: HList as -> tuple

instance ListTuple () '[] where
  tupleToList _ = Nil
  listToTuple _ = ()

instance ListTuple a '[a] where
  tupleToList a           = a :> Nil
  listToTuple (a :> Nil)  = a

instance ListTuple (a, b) '[a, b] where
  tupleToList (a, b)          = a :> b :> Nil
  listToTuple (a :> b :> Nil) = (a, b)

instance ListTuple (a, b, c) '[a, b, c] where
  tupleToList (a, b, c)            = a :> b :> c :> Nil
  listToTuple (a :> b :> c :> Nil) = (a, b, c)

instance ListTuple (a, b, c, d) '[a, b, c, d] where
  tupleToList (a, b, c, d)              = a :> b :> c :> d:> Nil
  listToTuple (a :> b :> c :> d :> Nil) = (a, b, c, d)

instance ListTuple (a, b, c, d, e) '[a, b, c, d, e] where
  tupleToList (a, b, c, d, e)                = a :> b :> c :> d:> e :> Nil
  listToTuple (a :> b :> c :> d :> e :> Nil) = (a, b, c, d, e)

instance ListTuple (a, b, c, d, e, f) '[a, b, c, d, e, f] where
  tupleToList (a, b, c, d, e, f)                  = a :> b :> c :> d:> e :> f :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> Nil) = (a, b, c, d, e, f)

instance ListTuple (a, b, c, d, e, f, g) '[a, b, c, d, e, f, g] where
  tupleToList (a, b, c, d, e, f, g)                    = a :> b :> c :> d:> e :> f :> g :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> Nil) = (a, b, c, d, e, f, g)

instance ListTuple (a, b, c, d, e, f, g, h) '[a, b, c, d, e, f, g, h] where
  tupleToList (a, b, c, d, e, f, g, h)                      = a :> b :> c :> d:> e :> f :> g :> h :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> Nil) = (a, b, c, d, e, f, g, h)

class GCollectible (f :: Type -> Type) (as :: [Type]) | f -> as where
  gtoCollection   :: f x -> HList as
  gfromCollection :: HList as -> f x

instance
  ( GCollectible l as
  , GCollectible r bs
  , cs ~ (as ++ bs)
  , Splittable as bs cs
  ) => GCollectible (l :*: r) cs where

  gtoCollection (l :*: r) = gtoCollection l `append` gtoCollection r
  gfromCollection cs      = let (as, bs) = split @as @bs cs in gfromCollection as :*: gfromCollection bs

instance GCollectible (K1 R a) '[a] where
  gtoCollection (K1 x)       = x :> Nil
  gfromCollection (x :> Nil) = K1 x

instance GCollectible U1 '[] where
  gtoCollection U1    = Nil
  gfromCollection Nil = U1

instance GCollectible f as => GCollectible (M1 S meta f) as where
  gtoCollection (M1 x) = gtoCollection x
  gfromCollection xs   = M1 (gfromCollection xs)

instance GCollectible f as => GCollectible (M1 D meta f) as where
  gtoCollection (M1 x) = gtoCollection x
  gfromCollection xs   = M1 (gfromCollection xs)

instance GCollectible f as => GCollectible (M1 C meta f) as where
  gtoCollection (M1 x) = gtoCollection x
  gfromCollection xs   = M1 (gfromCollection xs)
