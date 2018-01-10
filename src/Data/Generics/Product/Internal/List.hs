{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

#if __GLASGOW_HASKELL__ == 802
{-# OPTIONS_GHC -fno-solve-constant-dicts #-}
#endif


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
  , type (++)
  , Elem
  , ListTuple (..)
  ) where

import GHC.TypeLits
import Data.Generics.Internal.Lens

import Data.Kind    (Type)
import GHC.Generics

data List (as :: [(m, Type)]) where
  Nil :: List '[]
  (:>) :: a -> List as -> List ('(s, a) ': as)

infixr 5 :>

type family ((as :: [k]) ++ (bs :: [k])) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': as ++ bs

instance Monoid (List '[]) where
  mempty  = Nil
  mappend _ _ = Nil

instance (Monoid a, Monoid (List as)) => Monoid (List ('(k, a) ': as)) where
  mempty = mempty :> mempty
  mappend (x :> xs) (y :> ys) = mappend x y :> mappend xs ys

class Elem (as :: [(k, Type)]) (key :: k) (i :: Nat) a | as key -> i a
instance {-# OVERLAPPING #-} pos ~ 0 => Elem ('(key, a) ': xs) key pos a
instance (Elem xs key i a, pos ~ (i + 1)) => Elem (x ': xs) key pos a

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

instance GIsList m f g as bs => GIsList m (M1 t meta f) (M1 t meta g) as bs where
  glist = mIso . glist @m

instance {-# OVERLAPS #-}
  GIsList Symbol
          (S1 ('MetaSel ('Just field) u s i) (Rec0 a))
          (S1 ('MetaSel ('Just field) u s i) (Rec0 b))
          '[ '(field, a)] '[ '(field, b)] where
  glist = mIso . kIso . singleton

instance GIsList Type (Rec0 a) (Rec0 a) '[ '(a, a)] '[ '(a, a)] where
  glist = kIso . singleton

instance GIsList () (Rec0 a) (Rec0 b) '[ '( '(), a)] '[ '( '(), b)] where
  glist = kIso . singleton

instance GIsList m U1 U1 '[] '[] where
  glist = iso (const Nil) (const U1)

--------------------------------------------------------------------------------
-- as ++ bs == cs
-- as' + == cs'
class Appending f (as :: [k]) bs cs (as' :: [k]) bs' cs' | as bs cs cs' -> as' bs', as' bs' cs cs' -> as bs, as bs -> cs, as' bs' -> cs' where
  appending :: Iso (f as, f bs) (f as', f bs') (f cs) (f cs')

instance Appending List '[] bs bs '[] bs' bs' where
  appending = iso snd (Nil,)

instance Appending List as bs cs as' bs' cs'
  => Appending List ('(f, a) ': as) bs ('(f, a) ': cs) ('(f, a') ': as') bs' ('(f, a') ': cs') where
  appending = pairing (fromIso consing) id . assoc3 . pairing id appending . consing

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

--------------------------------------------------------------------------------
-- * Convert tuples to/from HLists

class ListTuple (tuple :: Type) (as :: [(k, Type)]) | as -> tuple where
  type ListToTuple as :: Type
  tupleToList :: tuple -> List as
  listToTuple :: List as -> tuple

instance ListTuple () '[] where
  type ListToTuple '[] = ()
  tupleToList _ = Nil
  listToTuple _ = ()

instance ListTuple a '[ '(fa, a)] where
  type ListToTuple '[ '(fa, a)] = a
  tupleToList a
    = a :> Nil
  listToTuple (a :> Nil)
    = a

instance ListTuple (a, b) '[ '(fa, a), '(fb, b)] where
  type ListToTuple '[ '(fa, a), '(fb, b)] = (a, b)
  tupleToList (a, b)
    = a :> b :> Nil
  listToTuple (a :> b :> Nil)
    = (a, b)

instance ListTuple (a, b, c) '[ '(fa, a), '(fb, b), '(fc, c)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c)] = (a, b, c)
  tupleToList (a, b, c)
    = a :> b :> c :> Nil
  listToTuple (a :> b :> c :> Nil)
    = (a, b, c)

instance ListTuple (a, b, c, d) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d)] = (a, b, c, d)
  tupleToList (a, b, c, d)
    = a :> b :> c :> d:> Nil
  listToTuple (a :> b :> c :> d :> Nil)
    = (a, b, c, d)

instance ListTuple (a, b, c, d, e) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e)] = (a, b, c, d, e)
  tupleToList (a, b, c, d, e)
    = a :> b :> c :> d:> e :> Nil
  listToTuple (a :> b :> c :> d :> e :> Nil)
    = (a, b, c, d, e)

instance ListTuple (a, b, c, d, e, f) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f)] = (a, b, c, d, e, f)
  tupleToList (a, b, c, d, e, f)
    = a :> b :> c :> d:> e :> f :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> Nil)
    = (a, b, c, d, e, f)

instance ListTuple (a, b, c, d, e, f, g) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g)] = (a, b, c, d, e, f, g)
  tupleToList (a, b, c, d, e, f, g)
    = a :> b :> c :> d:> e :> f :> g :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> Nil)
    = (a, b, c, d, e, f, g)

instance ListTuple (a, b, c, d, e, f, g, h) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h)] = (a, b, c, d, e, f, g, h)
  tupleToList (a, b, c, d, e, f, g, h)
    = a :> b :> c :> d:> e :> f :> g :> h :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> Nil)
    = (a, b, c, d, e, f, g, h)

instance ListTuple (a, b, c, d, e, f, g, h, j) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j)] = (a, b, c, d, e, f, g, h, j)
  tupleToList (a, b, c, d, e, f, g, h, j)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> Nil)
    = (a, b, c, d, e, f, g, h, j)

instance ListTuple (a, b, c, d, e, f, g, h, j, k) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k)] = (a, b, c, d, e, f, g, h, j, k)
  tupleToList (a, b, c, d, e, f, g, h, j, k)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> Nil)
    = (a, b, c, d, e, f, g, h, j, k)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l)] = (a, b, c, d, e, f, g, h, j, k, l)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m)] = (a, b, c, d, e, f, g, h, j, k, l, m)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n)] = (a, b, c, d, e, f, g, h, j, k, l, m, n)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o)] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o), '(fp, p)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o), '(fp, p)] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o), '(fp, p), '(fq, q)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o), '(fp, p), '(fq, q)] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o), '(fp, p), '(fq, q), '(fr, r)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o), '(fp, p), '(fq, q), '(fr, r)] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> r :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> r :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s) '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o), '(fp, p), '(fq, q), '(fr, r), '(fs, s)] where
  type ListToTuple '[ '(fa, a), '(fb, b), '(fc, c), '(fd, d), '(fe, e), '(ff, f), '(fg, g), '(fh, h), '(fj, j), '(fk, k), '(fl, l), '(fm, m), '(fn, n), '(fo, o), '(fp, p), '(fq, q), '(fr, r), '(fs, s)] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> r :> s :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> r :> s :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s)
