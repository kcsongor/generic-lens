{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Typed
-- Copyright   :  (C) 2017 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-field-type-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Typed
  ( -- *Prisms
    --
    --  $example
    AsType (..)
  ) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits (TypeError, ErrorMessage (..))
import Data.Generics.Sum.Internal.Typed

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

--  $example
--  @
--    module Example where
--
--    import Data.Generics.Sum
--    import GHC.Generics
--
--    data Animal
--      = Dog Dog
--      | Cat Name Age
--      | Duck Age
--      deriving (Generic, Show)
--
--    data Dog = MkDog
--      { name :: Name
--      , age  :: Age
--      }
--      deriving (Generic, Show)
--
--    type Name = String
--    type Age  = Int
--
--    dog, cat, duck :: Animal
--
--    dog = Dog (MkDog "Shep" 3)
--    cat = Cat "Mog" 5
--    duck = Duck 2
--  @

-- |Sums that have a constructor with a field of the given type.
class AsType a s where
  -- |A prism that projects a constructor uniquely identifiable by the type of
  --  its field. Compatible with the lens package's 'Control.Lens.Prism' type.
  --
  --  >>> dog ^? _Typed @Dog
  --  Just (MkDog {name = "Shep", age = 3})
  --  >>> dog ^? _Typed @Age
  --  Nothing
  --  >>> cat ^? _Typed @(Name, Age)
  --  Just ("Mog",5)
  --  >>> duck ^? _Typed @Age
  --  Just 2
  _Typed :: Prism' s a

  -- |Inject by type.
  injectTyped :: a -> s

  -- |Project by type.
  projectTyped :: s -> Maybe a

instance
  ( Generic s
  , ErrorUnlessOne a s (CountPartialType a (Rep s))
  , GAsType (Rep s) a
  ) => AsType a s where

  _Typed
    = repIso . _GTyped
  injectTyped
    = to . ginjectTyped
  projectTyped
    = either (const Nothing) Just . gprojectTyped . from

type family ErrorUnlessOne (a :: Type) (s :: Type) (count :: Count) :: Constraint where
  ErrorUnlessOne a s 'None
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a constructor whose field is of type "
        ':<>: 'ShowType a
        )

  ErrorUnlessOne a s 'Multiple
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " contains multiple constructors whose fields are of type "
        ':<>: 'ShowType a
        ':<>: 'Text "; the choice of constructor is thus ambiguous"
        )

  ErrorUnlessOne _ _ 'One
    = ()
