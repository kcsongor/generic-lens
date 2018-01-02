{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Data.Generics.Product.Internal.List () where

  -- ( GIsList (..)
  -- ) where

import GHC.TypeLits
import Data.Generics.Internal.HList (type (++), Splittable (..))
import Data.Generics.Internal.Lens
import Data.Generics.Internal.Void

import Data.Kind    (Type)
import GHC.Generics

data List (as :: [(Symbol, Type)]) where
  Nil :: List '[]
  (:>) :: a -> List as -> List ('(s, a) ': as)

infixr 5 :>

instance (Show a, Show (List as)) => Show (List ('(s, a) ': as)) where
  show (x :> xs) = show x ++ " :> " ++ show xs

instance Show (List '[]) where
  show _ = "Nil"

head' :: List ('(f, a) ': as) -> a
head' (x :> _) = x

append' :: List as -> List bs -> List (as ++ bs)
append' Nil ys = ys
append' (x :> xs) ys = x :> (append' xs ys)

class GIsList
  (f :: Type -> Type)
  (g :: Type -> Type)
  (as :: [(Symbol, Type)])
  (bs :: [(Symbol, Type)]) | f -> as, g -> bs, bs f -> g, as g -> f where

  -- TODO: Iso?
  glist :: Lens (f x) (g x) (List as) (List bs)
  glist' :: Lens (List as) (List bs) (f x) (g x)

instance
  ( GIsList l l as as
  , GIsList l l' as as'
  , GIsList r r bs bs
  , GIsList r r' bs bs'
  , GIsList l' l' as' as'
  , GIsList r' r' bs' bs'
  , SameSplit as bs cs as' bs' cs'
  , cs ~ (as ++ bs)
  , cs' ~ (as' ++ bs')
  , Splittable List as bs cs
  , Splittable List as' bs' cs'
  ) => GIsList (l :*: r) (l' :*: r') cs cs' where

  -- TODO: make split type-changing so this can be written as a single lens.
  glist f (l :*: r) = (\(a, b) -> a ^. glist' :*: b ^. glist') . split <$> f (append' (l ^. glist) (r ^. glist))

  glist' f xs = (\(l :*: r) -> append' (l ^. glist) (r ^. glist))
                <$> f (l' ^. glist' :*: r' ^. glist')
    where (l', r') = split xs

-- as ++ bs == cs
-- as' ++ bs' == cs'
class SameSplit (as :: [k]) (bs :: [k]) (cs :: [k]) (as' :: [k]) (bs' :: [k]) (cs' :: [k]) | as bs cs cs' -> as' bs', as' bs' cs cs' -> as bs, as bs -> cs, as' bs' -> cs'
instance SameSplit '[] bs bs '[] bs' bs'
instance SameSplit xs bs cs xs' bs' cs'
  => SameSplit (x ': xs) bs (x ': cs) (x' ': xs') bs' (x' ': cs')

instance {-# OVERLAPS #-}
  GIsList (S1 ('MetaSel ('Just field) u s i) (Rec0 a))
          (S1 ('MetaSel ('Just field) u s i) (Rec0 b))
          '[ '(field, a)] '[ '(field, b)] where
  glist = mLens . kLens . (\f a -> head' <$> f (a :> Nil))
  glist' f (as :> Nil) = (:> Nil) <$> f' as
    where f'  = fmap (unK1 . unM1) . f . M1 . K1

instance GIsList U1 U1 '[] '[] where
  glist f u = u <$ f Nil
  glist' f u = u <$ f U1

instance GIsList f g as bs => GIsList (M1 m meta f) (M1 m meta g) as bs where
  glist = mLens . glist
  glist' f as = glist' (fmap unM1 . f . M1) as

--------------------------------------------------------------------------------
class IsList
  (f :: Type)
  (g :: Type)
  (as :: [(Symbol, Type)])
  (bs :: [(Symbol, Type)]) | f -> as, g -> bs where

  -- TODO: Iso?
  list :: Lens f g (List as) (List bs)
  list' :: Lens (List as) (List bs) f g

instance
  ( Generic f
  , Generic g
  , GIsList (Rep f) (Rep g) as bs
  ) => IsList f g as bs where
  list = repLens . glist
  list' f xs = glist' (fmap from . f . to) xs

data Test = Test
  { foo :: Int
  , bar :: String
  } deriving Generic

data Test2 = Test2
  { foo :: Int
  , bar :: String
  , baz :: Char
  } deriving Generic

class Extend f g as | f g -> as where
  extend :: f -> List as -> g

instance
  ( IsList f f fs fs
  , cs ~ (fs ++ as)
  , IsList g g cs cs
  , PrefixOf fs cs as
  ) => Extend f g as where

  extend f as = ((f ^. list) `append'` as) ^. list'

class PrefixOf as bs cs | as bs -> cs
instance PrefixOf '[] cs cs
instance PrefixOf xs ys cs => PrefixOf (x ': xs) (x ': ys) cs

--------------------------------------------------------------------------------

instance Splittable List '[] bs bs where
  split bs = (Nil, bs)

instance Splittable List as bs cs => Splittable List (a ': as) bs (a ': cs) where
  split (a :> as)
    = (a :> as', bs)
    where (as', bs) = split as

-- type family Length xs where
--   Length '[] = 0
--   Length (x ': xs) = 1 + Length xs

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
