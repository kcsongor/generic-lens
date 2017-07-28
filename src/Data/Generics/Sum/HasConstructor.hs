{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.HasConstructor
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive prisms generically
--
-- @
--
-- data FooBar
--   = Foo Int
--   | Bar Int String
--   | Baz (Int, String)
--   | Qux
--   deriving (Show, Generic)
--
-- @
--
-- >>> Bar 5 "coconut" ^? as @"Bar"
-- Just (5, "coconut")
--
-- >>> Qux ^? as @"Qux"
-- Just ()
--
-- >>> as @"Bar" # (5, "asd") :: FooBar
-- Bar 5 "asd"
--

-----------------------------------------------------------------------------

module Data.Generics.Sum.HasConstructor
  ( HasConstructor (..)
  ) where

import Data.Generics.Internal.Lens

import GHC.TypeLits             (Symbol, TypeError, ErrorMessage (..))
import Data.Kind                (Type, Constraint)
import GHC.Generics

class HasConstructor (con :: Symbol) a s | s con -> a where
  as :: Prism' s a

instance
  ( Generic s
  , ErrorUnless (ContainsC con (Rep s)) con s
  , GHasConstructor con (Rep s) a
  ) => HasConstructor con a s where
  as =  repIso . gconstruct @con

--------------------------------------------------------------------------------

type family ErrorUnless (contains :: Bool) (con :: Symbol) (s :: *) :: Constraint where
  ErrorUnless 'False con s
    = TypeError ( 'ShowType s
                ':<>: 'Text " has no constructor with name "
                ':<>: 'ShowType con)
  ErrorUnless _ _ _
    = ()

--------------------------------------------------------------------------------

data HList (xs :: [Type]) where
  Nil  :: HList '[]
  (:>) :: x -> HList xs -> HList (x ': xs)

infixr 5 :>

append :: HList xs -> HList ys -> HList (xs ++ ys)
append Nil ys = ys
append (x :> xs) ys = x :> (xs `append` ys)

class AsList (tup :: *) (xs :: [*]) | xs -> tup where
  asList  :: tup -> HList xs
  asTuple :: HList xs -> tup

instance AsList () '[] where
  asList _
    = Nil

  asTuple _
    = ()

instance AsList a '[a] where
  asList a
    = a :> Nil

  asTuple (a :> Nil)
    = a

instance AsList (a, b) '[a, b] where
  asList (a, b)
    = a :> b :> Nil

  asTuple (a :> b :> Nil)
    = (a, b)

instance AsList (a, b, c) '[a, b, c] where
  asList (a, b, c)
    = a :> b :> c :> Nil

  asTuple (a :> b :> c :> Nil)
    = (a, b, c)

instance AsList (a, b, c, d) '[a, b, c, d] where
  asList (a, b, c, d)
    = a :> b :> c :> d :> Nil

  asTuple (a :> b :> c :> d :> Nil)
    = (a, b, c, d)

instance AsList (a, b, c, d, e) '[a, b, c, d, e] where
  asList (a, b, c, d, e)
    = a :> b :> c :> d :> e :> Nil

  asTuple (a :> b :> c :> d :> e :> Nil)
    = (a, b, c, d, e)

instance AsList (a, b, c, d, e, f) '[a, b, c, d, e, f] where
  asList (a, b, c, d, e, f)
    = a :> b :> c :> d :> e :> f :> Nil

  asTuple (a :> b :> c :> d :> e :> f :> Nil)
    = (a, b, c, d, e, f)

instance AsList (a, b, c, d, e, f, g) '[a, b, c, d, e, f, g] where
  asList (a, b, c, d, e, f, g)
    = a :> b :> c :> d :> e :> f :> g :> Nil

  asTuple (a :> b :> c :> d :> e :> f :> g :> Nil)
    = (a, b, c, d, e, f, g)

instance AsList (a, b, c, d, e, f, g, h) '[a, b, c, d, e, f, g, h] where
  asList (a, b, c, d, e, f, g, h)
    = a :> b :> c :> d :> e :> f :> g :> h :> Nil

  asTuple (a :> b :> c :> d :> e :> f :> g :> h :> Nil)
    = (a, b, c, d, e, f, g, h)

type family ((xs :: [k]) ++ (ys :: [k])) :: [k] where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': xs ++ ys

--------------------------------------------------------------------------------

class GHasConstructor (con :: Symbol) (s :: Type -> Type) a | con s -> a where
  gconstruct :: Prism' (s x) a

instance GHasConstructor con s a => GHasConstructor con (M1 D c s) a where
  gconstruct = mIso . gconstruct @con

instance (GCollect s xs, AsList a xs) => GHasConstructor con (M1 C ('MetaCons con f n) s) a where
  gconstruct = prism (M1 . gto . asList) (Right . asTuple @_ @xs . gfrom . unM1)

instance (GHasConstructor con s a) => GHasConstructor con (M1 S m s) a where
  gconstruct = mIso . gconstruct @con

class GHasConstructorSum (con :: Symbol) l r a (p :: Bool) | con l r p -> a where
  gconstructSum :: Prism' ((l :+: r) x) a

instance GHasConstructor con l a => GHasConstructorSum con l r a 'True where
  gconstructSum = left . gconstruct @con

instance GHasConstructor con r a => GHasConstructorSum con l r a 'False where
  gconstructSum = right . gconstruct @con

instance GHasConstructorSum con l r a (ContainsC con l) => GHasConstructor con (l :+: r) a where
  gconstruct = gconstructSum @con @l @r @a @(ContainsC con l)

--------------------------------------------------------------------------------
-- * Split is the inverse of append

class Split xs ys zs | xs ys -> zs, xs zs -> ys where
  split :: HList zs -> (HList xs, HList ys)

instance Split '[] xs xs where
  split xs = (Nil, xs)

instance Split xs ys zs => Split (x ': xs) ys (x ': zs) where
  split (x :> xs) = (x :> xs', ys')
    where (xs', ys') = split xs

--------------------------------------------------------------------------------

-- | Collect parameters of a constructor into an HList
class GCollect (s :: Type -> Type) (xs :: [Type]) | s -> xs where
  gfrom :: s x -> HList xs
  gto :: HList xs -> s x

instance
  ( GCollect a as
  , GCollect b bs
  , xs ~ (as ++ bs)
  , Split as bs xs
  ) => GCollect (a :*: b) xs where

  gfrom (a :*: b) = gfrom a `append` gfrom b
  gto xs = gto l :*: gto r
    where (l, r) = split @as @bs xs

instance GCollect (K1 R a) '[a] where
  gfrom (K1 x) = x :> Nil
  gto (x :> Nil) = K1 x

instance GCollect U1 '[] where
  gfrom U1 = Nil
  gto Nil  = U1

instance GCollect s a => GCollect (M1 S c s) a where
  gfrom (M1 x) = gfrom x
  gto x = M1 $ gto x

instance GCollect s a => GCollect (M1 D c s) a where
  gfrom (M1 x) = gfrom x
  gto x = M1 $ gto x

instance GCollect s a => GCollect (M1 C c s) a where
  gfrom (M1 x) = gfrom x
  gto x = M1 $ gto x

--------------------------------------------------------------------------------

type family ContainsC (cname :: Symbol) f :: Bool where
  ContainsC cname (C1 ('MetaCons cname _ _) _)
    = 'True
  ContainsC cname (f :+: g)
    = ContainsC cname f || ContainsC cname g
  ContainsC cname (D1 m f)
    = ContainsC cname f
  ContainsC cname _
    = 'False

-- | Type-level alternative
type family (a :: Bool) || (b :: Bool) :: Bool where
  'True  || _  = 'True
  'False || b = b

