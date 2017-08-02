{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE DeriveGeneric   #-}

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

    -- *Internals
  , GAsType (..)
  ) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits (TypeError, ErrorMessage (..))

import Data.Generics.Internal.Families
import Data.Generics.Internal.HList
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

instance
  ( Generic s
  , ErrorUnlessOne a s (CountPartialType a (Rep s))
  , GAsType (Rep s) a
  ) => AsType a s where

  _Typed = repIso . _GTyped

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

-- |As 'AsType' but over generic representations as defined by "GHC.Generics".
class GAsType (f :: Type -> Type) a where
  _GTyped :: Prism' (f x) a
  _GTyped = prism ginjectTyped gprojectTyped

  ginjectTyped  :: a -> f x
  gprojectTyped :: f x -> Either (f x) a

instance
  ( GCollectible f as
  , ListTuple a as
  ) => GAsType (M1 C meta f) a where

  ginjectTyped
    = M1 . gfromCollection . tupleToList
  gprojectTyped
    = Right . listToTuple . gtoCollection . unM1

instance GSumAsType (HasPartialTypeTupleP a l) l r a => GAsType (l :+: r) a where
  ginjectTyped
    = ginjectSumTyped @(HasPartialTypeTupleP a l) @l @r @a
  gprojectTyped
    = gprojectSumTyped @(HasPartialTypeTupleP a l) @l @r @a

instance GAsType f a => GAsType (M1 D meta f) a where
  ginjectTyped
    = M1 . ginjectTyped
  gprojectTyped
    = either (Left . M1) Right . gprojectTyped . unM1

class GSumAsType (contains :: Bool) l r a where
  _GSumTyped :: Prism' ((l :+: r) x) a
  _GSumTyped = prism (ginjectSumTyped  @contains) (gprojectSumTyped @contains)

  ginjectSumTyped  :: a -> (l :+: r) x
  gprojectSumTyped :: (l :+: r) x -> Either ((l :+: r) x) a

instance GAsType l a => GSumAsType 'True l r a where
  ginjectSumTyped
    = L1 . ginjectTyped
  gprojectSumTyped
    = \x -> case x of
        L1 l -> either (Left . L1) Right (gprojectTyped l)
        R1 _ -> Left x

instance GAsType r a => GSumAsType 'False l r a where
  ginjectSumTyped
    = R1 . ginjectTyped
  gprojectSumTyped
    = \x -> case x of
        R1 r -> either (Left . R1) Right (gprojectTyped r)
        L1 _ -> Left x
