{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Labels
-- Copyright   : (C) 2020 Csongor Kiss
-- License     : BSD3
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Plugs in support for creating generics based lenses and prisms with
-- @#identifiers@ from the @OverloadedLabels@ extension.  Note that since
-- overloaded labels do not support symbols starting with capital letters, all
-- prisms (which come from constructor names, which are capitalized) must be
-- prefixed with an underscore (e.g. @#_ConstructorName@).
--------------------------------------------------------------------------------

module Data.Generics.Labels
  ( Field(..)
  , Field'
  , Constructor(..)
  , Constructor'
  ) where

import Data.Generics.Product.Fields
import Data.Generics.Sum.Constructors

import Optics.Internal.Optic
import Optics.Lens (Lens)
import Optics.Prism (Prism)

import Data.Type.Equality
import Data.Type.Bool
import GHC.TypeLits

type family GenericOpticKind (name :: Symbol) :: OpticKind where
  GenericOpticKind name =
    If (CmpSymbol "_@" name == 'LT && CmpSymbol "_[" name == 'GT)
      A_Prism
      A_Lens

instance
  ( k ~ GenericOpticKind name
  , GenericOptic name k s t a b
  ) => GeneralLabelOptic name k s t a b 'RepDefined where
  generalLabelOptic = genericOptic @name @k @s @t @a @b

----------------------------------------

-- | This helper class allows us to customize the output type of the lens to be
-- either 'Prism' or 'Lens' (by choosing appropriate @k@).  The choice is made
-- by the 'GenericOpticKind' type family based on whether the symbol starts with
-- an underscore followed by a capital letter (a check done in the
-- 'GenericLabelOptic' instance above).  If so, then we're dealing with a
-- constructor name, which should be a prism, and otherwise, it's a field name,
-- so we have a lens.
class GenericOptic name k s t a b where
  genericOptic :: Optic k NoIx s t a b

instance Field name s t a b => GenericOptic name A_Lens s t a b where
  genericOptic = fieldLens @name @s @t @a @b

instance
  ( Constructor name s t a b
  , _name ~ AppendSymbol "_" name
  ) => GenericOptic _name A_Prism s t a b where
  genericOptic = constructorPrism @name @s @t @a @b

----------------------------------------

-- | 'Field' is morally the same as 'HasField', but it is constructed from an
-- incoherent combination of 'HasField' and 'HasField''. In this way, it can be
-- seamlessly used even when dealing with data types that don't have 'HasField'
-- instances (like data instances).
class Field name s t a b where
  fieldLens :: Lens s t a b

type Field' name s a = Field name s s a a

instance {-# INCOHERENT #-} HasField name s t a b => Field name s t a b where
  fieldLens = field @name @s @t @a @b

-- | Use 'field'' when appropriate for faster compile times. Note that if @a ~
-- b@, then necessarily @s ~ t@. This doesn't apply in general because of
-- phantom type variables, but 'LabelOptic' doesn't support type changing
-- updates for them.
instance (HasField' name s a, s ~ t) => Field name s t a a where
  fieldLens = field' @name @s @a

----------------------------------------

-- | 'Constructor' is morally the same as 'AsConstructor', but it is constructed
-- from an incoherent combination of 'AsConstructor' and 'AsConstructor''. In
-- this way, it can be seamlessly used even when dealing with data types that
-- don't have 'AsConstructor' instances (like data instances).
class Constructor name s t a b where
  constructorPrism :: Prism s t a b

type Constructor' name s a = Constructor name s s a a

instance {-# INCOHERENT #-} AsConstructor name s t a b => Constructor name s t a b where
  constructorPrism = _Ctor @name @s @t @a @b

-- | Use '_Ctor'' when appropriate for faster compile times. Note that if @a ~
-- b@, then necessarily @s ~ t@. This doesn't apply in general because of
-- phantom type variables, but 'LabelOptic' doesn't support type changing
-- updates for them.
instance (AsConstructor' name s a, s ~ t) => Constructor name s t a a where
  constructorPrism = _Ctor' @name @s @a
