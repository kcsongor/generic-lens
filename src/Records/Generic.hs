{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Records.Generic
-- Copyright   :  (C) 2016 Csongor Kiss
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
    -- * Magic getter
    HasField (..)

    -- * Subtype relationship
  , Subtype  (..)
  ) where

import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Generics
import Control.Applicative ((<|>))

--------------------------------------------------------------------------------

-- |Type-level list append
type family ((xs :: [k]) ++ (ys :: [k])) :: [k] where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

type family (Collect a) :: [(Symbol, Type)] where
  Collect (D1 m cs)
    = Collect cs
  Collect (C1 mc cs)
    = Collect cs
  Collect (a :*: b)
    = Collect a ++ Collect b
  Collect (S1 ('MetaSel ('Just n) p f b) (Rec0 t))
    = '[ '(n, t) ]
  Collect x = TypeError ('Text "Invalid type")

class GHasField rec (field :: Symbol) a where
  ggetField :: rec x -> Proxy field -> a

instance
  ( Contains field (Collect rec) ~ 'Just a
  , LookupField rec field a
  ) => GHasField rec field a where
  ggetField rec p = res
    where Just res = lookupField rec p :: Maybe a

-- This could be kind polymorphic, but the error message implies this kind anyway
type family Contains (sym :: Symbol) (xs :: [(Symbol, Type)]) :: Maybe Type where
  Contains s ('(s, t) ': _)
    = 'Just t
  Contains s (_ ': xs)
    = Contains s xs
  Contains s _
    = TypeError ('Text "Field " ':<>: 'ShowType s ':<>: 'Text " not found in source record")

{-
  TODO: It's be possible to determine which branch contains the field
  (e.g. with a type family) and then we won't need the `Maybe` business.

  That will possibly require an extra argument to differentiate between the two
  cases in the instance definition (and not just the constraints thereof) to
  avoid duplicate instances
-}
class LookupField a field ret where
  lookupField :: a x -> Proxy field -> Maybe ret

instance (LookupField a field ret, LookupField b field ret) => LookupField (a :*: b) field ret where
  lookupField (a :*: b) p = lookupField a p <|> lookupField b p

instance LookupField (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) field t where
  lookupField (M1 x) = lookupField x

instance {-# OVERLAPS #-} LookupField (S1 ('MetaSel ms su ss ds) t) field x where
  lookupField _ _ = Nothing

instance {-# OVERLAPS #-} LookupField f field ret => LookupField (M1 i c f) field ret where
  lookupField (M1 x) = lookupField x

instance LookupField (K1 R a) field a where
  lookupField (K1 x) _ = Just x

--------------------------------------------------------------------------------
class Convert (rep :: Type -> Type) (f :: Type -> Type) where
  convert :: rep p -> f p

instance (Convert rep a, Convert rep b) => Convert rep (a :*: b) where
  convert rep = (convert rep) :*: (convert rep)

instance GHasField rep field t => Convert rep (S1 ('MetaSel ('Just field) p f b) (Rec0 t)) where
  convert rep = M1 (K1 (ggetField rep (Proxy @field)))

instance {-# OVERLAPS #-} Convert rep f => Convert rep (M1 i c f) where
  convert = M1 . convert
--------------------------------------------------------------------------------

-- Exported stuff

-- |Structural subtype relationship
class Subtype a b where
  upcast :: a -> b

instance (Convert (Rep a) (Rep b), Generic a, Generic b) => Subtype a b where
  upcast = to . convert . from

-- |Records that have a field with a given name
class Generic rec => HasField rec (field :: Symbol) a where
  getField :: rec -> Proxy field -> a

instance
  ( Contains field (Collect (Rep rec)) ~ 'Just a
  , Generic rec
  , LookupField (Rep rec) field a
  ) => HasField rec field a where
  getField rec p = res
    where Just res = lookupField (from rec) p :: Maybe a

