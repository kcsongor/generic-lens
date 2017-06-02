{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Records.Generic
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Magic record operations using Generics
--
-- These classes need not be instantiated manually, as GHC can automatically
-- prove valid instances via Generics. Only the `Generic` class needs to
-- be derived (see examples).
--
-----------------------------------------------------------------------------
module Records.Generic
  (
    -- * Magic lens
    HasField (..)

    -- * Getter and setter
  , getField
  , setField

    -- * Subtype relationship
  , Subtype  (..)
  ) where

import GHC.TypeLits             (Symbol, TypeError, ErrorMessage(..))
import Data.Kind                (Type)
import GHC.Generics

import Control.Applicative      (Const (..))

-- TODO: cleanup

--------------------------------------------------------------------------------
-- Lens helpers

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
s ^. l = getConst (l Const s)

set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)

first :: Lens' ((a :*: b) x) (a x)
first f (a :*: b) = fmap (:*: b) (f a)

second :: Lens' ((a :*: b) x) (b x)
second f (a :*: b) = fmap (a :*:) (f b)

type Iso' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

repIso :: Generic a => Iso' a (Rep a x)
repIso a = fmap to . a . from

lensM :: Lens' (M1 i c f p) (f p)
lensM f (M1 x) = fmap M1 (f x)

--------------------------------------------------------------------------------

class GHasField field s a | field s -> a where
  glabel :: Lens' (s x) a

instance (GHasFieldProd field s s' a (Contains field s)) => GHasField field (s :*: s') a where
  glabel = prodLabel @field @_ @_ @a @(Contains field s)

instance GHasField field (S1 ('MetaSel ('Just field) p f b) (Rec0 a)) a where
  glabel = lensM . glabel @field

instance GHasField field (K1 R a) a where
  glabel f (K1 x) = fmap K1 (f x)

instance GHasField field s a => GHasField field (M1 D c s) a where
  glabel = lensM . glabel @field

instance GHasField field s a => GHasField field (M1 C c s) a where
  glabel = lensM . glabel @field

--------------------------------------------------------------------------------

class GHasFieldProd field a b ret (w :: Maybe Type) | field a b -> ret where
  prodLabel :: Lens' ((a :*: b) x) ret

instance (GHasField field f ret) => GHasFieldProd field f g ret ('Just ret) where
  prodLabel = first . glabel @field

instance (GHasField field g ret) => GHasFieldProd field f g ret 'Nothing where
  prodLabel = second . glabel @field

--------------------------------------------------------------------------------

class Convert (rep :: Type -> Type) (f :: Type -> Type) where
  convert :: rep p -> f p

instance (Convert rep a, Convert rep b) => Convert rep (a :*: b) where
  convert rep = convert rep :*: convert rep

instance GHasField field rep t => Convert rep (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) where
  convert rep = M1 (K1 (rep ^. glabel @field))

instance {-# OVERLAPS #-} Convert rep f => Convert rep (M1 i c f) where
  convert = M1 . convert

--------------------------------------------------------------------------------

type family Contains (field :: Symbol) f :: Maybe Type where
  Contains field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'Just t
  Contains field (S1 _ _)
    = 'Nothing
  Contains field (D1 m f)
    = Contains field f
  Contains field (C1 m f)
    = Contains field f
  Contains field (f :*: g)
    = Contains field f <|> Contains field g
  Contains field (Rec0 _)
    = 'Nothing
  Contains field U1
    = 'Nothing
  Contains field V1
    = 'Nothing
  Contains x t = TypeError ('ShowType t)

type family (a :: Maybe k) <|> (b :: Maybe k) :: Maybe k where
  'Just x <|> _  = 'Just x
  _ <|> b = b

--------------------------------------------------------------------------------

-- Exported stuff

getField :: forall field a s. HasField field a s => s -> a
getField s = s ^. label @field

setField :: forall field a s. HasField field a s => a -> s -> s
setField = set (label @field)


-- |Structural subtype relationship
class Subtype a b where
  upcast :: a -> b

instance (Convert (Rep a) (Rep b), Generic a, Generic b) => Subtype a b where
  upcast = to . convert . from

-- |Records that have a field with a given name
class HasField (field :: Symbol) a s | s field -> a where
  label :: Lens' s a

instance
  ( Generic s
  , Contains field (Rep s) ~ 'Just a -- this is needed for the fundep for some reason
  , GHasField field (Rep s) a
  ) => HasField field a s where
  label =  repIso . glabel @field

-- Probably not a good idea to export this orphan...
--{-# LANGUAGE OverloadedLabels #-}
--import GHC.OverloadedLabels
--instance HasField field a s => IsLabel field (s -> a) where
--  fromLabel _ = (^. label @field)
