{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Product.Typed
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive lenses of a given type in a product.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Typed
  ( -- *Lenses
    --
    -- $setup
    HasType (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.VL.Lens as VL
import Data.Generics.Internal.Void
import Data.Generics.Product.Internal.Keyed

import Data.Kind    (Constraint, Type)
import GHC.Generics (Generic (Rep))
import GHC.TypeLits (TypeError, ErrorMessage (..))
import Data.Generics.Internal.Profunctor.Lens

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.Lens
-- >>> :{
-- data Human
--   = Human
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     , tall    :: Bool
--     }
--   | HumanNoTall
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     }
--   deriving (Generic, Show)
-- human :: Human
-- human = Human "Tunyasz" 50 "London" False
-- :}

-- |Records that have a field with a unique type.
class HasType a s where
  -- |A lens that focuses on a field with a unique type in its parent type.
  --  Compatible with the lens package's 'Control.Lens.Lens' type.
  --
  --  >>> human ^. typed @Int
  --  50
  --
  --  === /Type errors/
  --
  --  >>> human ^. typed @String
  --  ...
  --  ...
  --  ... The type Human contains multiple values of type [Char].
  --  ... The choice of value is thus ambiguous. The offending constructors are:
  --  ... Human
  --  ... HumanNoTall
  --  ...
  --
  --  >>> human ^. typed @Bool
  --  ...
  --  ...
  --  ... Not all constructors of the type Human contain a field of type Bool.
  --  ... The offending constructors are:
  --  ... HumanNoTall
  --  ...
  typed :: VL.Lens s s a a
  typed
    = VL.lens (getTyped @a) (uncurry (setTyped @a) . swap)

  -- |Get field at type.
  getTyped :: s -> a
  getTyped s = s ^. typed @a

  -- |Set field at type.
  setTyped :: a -> s -> s
  setTyped = VL.set (typed @a)

  {-# MINIMAL typed | setTyped, getTyped #-}

instance
  ( Generic s
  , ErrorUnlessOne a s (CollectTotalType a (Rep s))
  , GHasKey a (Rep s) (Rep s) a a
  ) => HasType a s where

  typed = ravel (repLens . gkey @a)

-- See Note [Uncluttering type signatures]
instance {-# OVERLAPPING #-} HasType a Void where
  typed = undefined

type family ErrorUnlessOne (a :: Type) (s :: Type) (stat :: TypeStat) :: Constraint where
  ErrorUnlessOne a s ('TypeStat '[_] '[] '[])
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a value of type "
        ':<>: 'ShowType a
        )

  ErrorUnlessOne a s ('TypeStat (n ': ns) _ _)
    = TypeError
        (     'Text "Not all constructors of the type "
        ':<>: 'ShowType s
        ':<>: 'Text " contain a field of type "
        ':<>: 'ShowType a ':<>: 'Text "."
        ':$$: 'Text "The offending constructors are:"
        ':$$: ShowSymbols (n ': ns)
        )

  ErrorUnlessOne a s ('TypeStat _ (m ': ms) _)
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " contains multiple values of type "
        ':<>: 'ShowType a ':<>: 'Text "."
        ':$$: 'Text "The choice of value is thus ambiguous. The offending constructors are:"
        ':$$: ShowSymbols (m ': ms)
        )

  ErrorUnlessOne _ _ ('TypeStat '[] '[] _)
    = ()
