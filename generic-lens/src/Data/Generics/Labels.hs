{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType           #-}
#endif
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-orphans        #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Data.Generics.Labels
-- Copyright   : (C) 2020 Csongor Kiss
-- License     : BSD3
-- Maintainer  : Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides an (orphan) IsLabel instance for field lenses and constructor
-- prisms, as well as positional lenses on GHC >=9.6.
-- Use at your own risk.
--------------------------------------------------------------------------------

module Data.Generics.Labels
  ( -- * Orphan IsLabel Instance
    -- $sec1
    Field(..)
  , Field'
  , Constructor(..)
  , Constructor'
  ) where

import "this" Data.Generics.Product
import "this" Data.Generics.Sum

import "this" Data.Generics.Internal.VL.Lens  (Lens)
import "this" Data.Generics.Internal.VL.Prism (Prism)

import Data.Profunctor    (Choice)
import Data.Type.Bool     (type (&&), If)
import Data.Type.Equality (type (==))

import GHC.OverloadedLabels
import GHC.TypeLits

-- $sec1
-- An instance for creating lenses and prisms with @#identifiers@ from the
-- @OverloadedLabels@ extension.  Note that since overloaded labels did not
-- support symbols starting with capital letters, all prisms (which come from
-- constructor names, which are capitalized) must be prefixed with an underscore
-- (e.g. @#_ConstructorName@) when you use a GHC older than 9.6.
--
-- Morally:
--
-- @
-- instance (HasField name s t a b) => IsLabel name (Lens s t a b) where ...
-- @
-- and
--
-- @
-- instance (AsConstructor name s t a b) => IsLabel name (Prism s t a b) where ...
-- @
--
-- Starting with GHC 9.6, you can also write e.g. @#2@ and @#15@ instead of
-- @position \@1@ and @position \@15@, so we morally have
--
-- @
-- instance (HasPosition i s t a b) => IsLabel (Show i) (Lens s t a b) where ...
-- @
--
--
-- Remember:
--
-- @
-- type Lens = forall f. Functor f => (a -> f b) -> s -> f t
--
-- type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
-- @
--
-- The orphan instance is unavoidable if we want to work with
-- lenses-as-functions (as opposed to a 'ReifiedLens'-like newtype).

-- | 'Field' is morally the same as 'HasField', but it is constructed from an
-- incoherent combination of 'HasField' and 'HasField''.  In this way, it can be
-- seamlessly used in the 'IsLabel' instance even when dealing with data types
-- that don't have 'Field' instances (like data instances).
class Field name s t a b | s name -> a, t name -> b, s name b -> t, t name a -> s where
  fieldLens :: Lens s t a b

type Field' name s a = Field name s s a a

instance {-# INCOHERENT #-} HasField name s t a b => Field name s t a b where
  fieldLens = field @name

instance {-# INCOHERENT #-} HasField' name s a => Field name s s a a where
  fieldLens = field' @name

-- | 'Constructor' is morally the same as 'AsConstructor', but it is constructed from an
-- incoherent combination of 'AsConstructor' and 'AsConstructor''.  In this way, it can be
-- seamlessly used in the 'IsLabel' instance even when dealing with data types
-- that don't have 'Constructor' instances (like data instances).
class Constructor name s t a b | name s -> a, name t -> b where
  constructorPrism :: Prism s t a b

type Constructor' name s a = Constructor name s s a a

instance {-# INCOHERENT #-} AsConstructor name s t a b => Constructor name s t a b where
  constructorPrism = _Ctor @name

instance {-# INCOHERENT #-} AsConstructor' name s a => Constructor name s s a a where
  constructorPrism = _Ctor' @name

data LabelType = FieldType | LegacyConstrType | ConstrType | PositionType

type family ClassifyLabel (name :: Symbol) :: LabelType where
  ClassifyLabel name =
    If (StartsWithDigit name)
      'PositionType
      ( If (CmpSymbol "_@" name == 'LT && CmpSymbol "_[" name == 'GT)
          'LegacyConstrType
          ( If (CmpSymbol "@" name == 'LT && CmpSymbol "[" name == 'GT)
              'ConstrType
              'FieldType
          )
      )

type StartsWithDigit name =
  CmpSymbol "/" name == 'LT && CmpSymbol ":" name == 'GT

instance ( labelType ~ ClassifyLabel name
         , IsLabelHelper labelType name p f s t a b
         , pafb ~ p a (f b), psft ~ p s (f t)) => IsLabel name (pafb -> psft) where
  fromLabel = labelOutput @labelType @name @p @f

-- | This helper class allows us to customize the output type of the lens to be
-- either 'Prism' or 'Lens' (by choosing appropriate @p@ and @f@) as well as to
-- choose between whether we're dealing with a lens or a prism.  The choice is
-- made by the @labelType@ argument, which is determined by whether the symbol
-- starts with a capital letter, optionally preceded by an underscore (a check
-- done in the 'IsLabel' instance above).  If so, then we're dealing with a
-- constructor name, which should be a prism, and otherwise, it's a field name,
-- so we have a lens.
--
-- On GHC >=9.6, we also check whether the symbol starts with a digit, in which
-- case we are dealing with an index for a positional lens.
class IsLabelHelper labelType name p f s t a b where
  labelOutput :: p a (f b) -> p s (f t)

instance (Functor f, Field name s t a b) => IsLabelHelper 'FieldType name (->) f s t a b where
  labelOutput = fieldLens @name

instance ( Applicative f, Choice p, Constructor name s t a b
         , name' ~ AppendSymbol "_" name) => IsLabelHelper 'LegacyConstrType name' p f s t a b where
  labelOutput = constructorPrism @name

instance ( Applicative f, Choice p, Constructor name s t a b
         ) => IsLabelHelper 'ConstrType name p f s t a b where
  labelOutput = constructorPrism @name

class Position (i :: Nat) s t a b | s i -> a, t i -> b, s i b -> t, t i a -> s where
  positionLens :: Lens s t a b

instance {-# INCOHERENT #-} HasPosition i s t a b => Position i s t a b where
  positionLens = position @i

instance {-# INCOHERENT #-} HasPosition' i s a => Position i s s a a where
  positionLens = position' @i

instance ( Functor f, Position i s t a b, i ~ ParseNat name
         ) => IsLabelHelper 'PositionType name (->) f s t a b where
  labelOutput = positionLens @i

-- 'ParseNat' is only necessary for positional lenses, which can only actually
-- be used with OverloadedLabels since GHC 9.6. Therefore, it is fine that this
-- code only compiles with GHC >=9.4 due to the use of newer GHC features (such
-- as 'UnconsSymbol').
#if MIN_VERSION_base(4,17,0)
type ParseNat name = ParseNat' 0 (UnconsSymbol name)

type family ParseNat' acc m where
  ParseNat' acc ('Just '(hd, tl)) =
    ParseNat' (10 * acc + DigitToNat hd) (UnconsSymbol tl)
  ParseNat' acc 'Nothing = acc

type DigitToNat c =
  If ('0' <=? c && c <=? '9')
    (CharToNat c - CharToNat '0')
    (TypeError ('Text "Invalid position number"))
#else
type family ParseNat name where
  ParseNat name = TypeError ('Text "Positional lenses not supported")
#endif
