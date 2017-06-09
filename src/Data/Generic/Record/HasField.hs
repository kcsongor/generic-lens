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
-- Module      :  Data.Generic.Record.HasField
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive record field getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generic.Record.HasField
  ( -- * Lens
    -- $example
    HasField (..)
    -- * Getter and setter functions
  , getField
  , setField
    -- * Internals
  , GHasField (..)
  ) where

import Data.Generic.Record.Lens

import GHC.TypeLits             (Symbol, TypeError, ErrorMessage(..))
import Data.Kind                (Type)
import GHC.Generics


-- $example
-- @
--
--   module Example where
--
--   import GHC.Generics
--   import Data.Generic.Record
--
--   data Human = Human
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     } deriving (Generic, Show)
--
--    human :: Human
--    human = Human \"Tunyasz\" 50 \"London\"
--
-- @

-- | Get 'field'
--
-- >>> getField @"name" human
-- "Tunyasz"
getField :: forall field a s. HasField field a s => s -> a
getField s = s ^. label @field

-- | Set 'field'
--
-- >>> setField @"age" (setField @"name" "Tamas" human) 30
-- Human {name = "Tamas", age = 30, address = "London"}
setField :: forall field a s. HasField field a s => a -> s -> s
setField = set (label @field)

-- | Records that have a field with a given name.
class HasField (field :: Symbol) a s | s field -> a where
  -- ^ Lens focusing on a field with a given name.
  --   Compatible with the lens package.
  --
  -- @
  --  type Lens' s a
  --    = forall f. Functor f => (a -> f a) -> s -> f s
  -- @
  --
  -- >>> human & label @"name" .~ "Tamas"
  -- Human {name = "Tamas", age = 50, address = "London"}
  label :: Lens' s a

-- | Instances are generated on the fly for all records that have the required
--   field.
instance
  ( Generic s
  , Contains field (Rep s) ~ 'Just a -- this is needed for the fundep for some reason
  , GHasField field (Rep s) a
  ) => HasField field a s where
  label =  repIso . glabel @field


class GHasFieldProd field a b ret (w :: Maybe Type) | field a b -> ret where
  prodLabel :: Lens' ((a :*: b) x) ret

instance (GHasField field f ret) => GHasFieldProd field f g ret ('Just ret) where
  prodLabel = first . glabel @field

instance (GHasField field g ret) => GHasFieldProd field f g ret 'Nothing where
  prodLabel = second . glabel @field

--------------------------------------------------------------------------------

-- | Look up a record field by name in the generic representation, and return
--   its corresponding type, if exists.
type family Contains (field :: Symbol) f :: Maybe Type where
  Contains field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t))
    = 'Just t
  Contains field (f :*: g)
    = Contains field f <|> Contains field g
  Contains field (S1 _ _)
    = 'Nothing
  Contains field (C1 m f)
    = Contains field f
  Contains field (D1 m f)
    = Contains field f
  Contains field (Rec0 _)
    = 'Nothing
  Contains field U1
    = 'Nothing
  Contains field V1
    = 'Nothing
  Contains x t = TypeError ('ShowType t)

-- | Type-level alternative
type family (a :: Maybe k) <|> (b :: Maybe k) :: Maybe k where
  'Just x <|> _  = 'Just x
  _ <|> b = b

--------------------------------------------------------------------------------

-- | Like 'HasField', but on the generic representation
class GHasField (field :: Symbol) (s :: Type -> Type) a | field s -> a where
  glabel :: Lens' (s x) a

instance (GHasFieldProd field s s' a (Contains field s)) => GHasField field (s :*: s') a where
  glabel = prodLabel @field @_ @_ @_ @(Contains field s)

instance GHasField field (S1 ('MetaSel ('Just field) p f b) (Rec0 a)) a where
  glabel = lensM . glabel @field

instance GHasField field (K1 R a) a where
  glabel f (K1 x) = fmap K1 (f x)

instance GHasField field s a => GHasField field (M1 D c s) a where
  glabel = lensM . glabel @field

instance GHasField field s a => GHasField field (M1 C c s) a where
  glabel = lensM . glabel @field
