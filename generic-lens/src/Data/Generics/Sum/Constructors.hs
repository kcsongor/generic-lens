{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Sum.Constructors
-- Copyright   :  (C) 2019 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-name-based prisms generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Sum.Constructors
  ( -- *Prisms

    -- $setup
    AsConstructor (..)
  , AsConstructor_ (..)
  , AsConstructor' (..)
  , AsConstructor0 (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Void
import Data.Generics.Sum.Internal.Constructors

import Data.Kind    (Constraint, Type)
import GHC.Generics (Generic (Rep))
import GHC.TypeLits (Symbol, TypeError, ErrorMessage (..))
import Data.Generics.Internal.VL.Prism
import qualified Data.Generics.Internal.Profunctor.Iso as P
import qualified Data.Generics.Internal.Profunctor.Prism as P
import Data.Generics.Internal.Errors

-- $setup
-- == /Running example:/
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies
-- >>> import GHC.Generics
-- >>> :m +Data.Generics.Internal.VL.Prism
-- >>> :m +Data.Generics.Product.Fields
-- >>> :m +Data.Function
-- >>> :{
-- data Animal a
--   = Dog (Dog a)
--   | Cat Name Age
--   | Duck Age
--   deriving (Generic, Show)
-- data Dog a
--   = MkDog
--   { name   :: Name
--   , age    :: Age
--   , fieldA :: a
--   }
--   deriving Show
-- type Name = String
-- type Age  = Int
-- dog, cat, duck :: Animal Int
-- dog = Dog (MkDog "Shep" 3 30)
-- cat = Cat "Mog" 5
-- duck = Duck 2
-- :}

-- |Sums that have a constructor with a given name.
class AsConstructor (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b where
  -- |A prism that projects a named constructor from a sum. Compatible with the
  --  lens package's 'Control.Lens.Prism' type.
  --
  --  >>> dog ^? _Ctor @"Dog"
  --  Just (MkDog {name = "Shep", age = 3, fieldA = 30})
  --
  --  >>> dog ^? _Ctor @"Cat"
  --  Nothing
  --
  --  >>> cat ^? _Ctor @"Cat"
  --  Just ("Mog",5)
  --
  --  >>> _Ctor @"Cat" # ("Garfield", 6) :: Animal Int
  --  Cat "Garfield" 6
  --
  --  === /Type errors/
  --
  --  >>> cat ^? _Ctor @"Turtle"
  --  ...
  --  ...
  --  ... The type Animal Int does not contain a constructor named "Turtle"
  --  ...
  _Ctor :: Prism s t a b

-- |Sums that have a constructor with a given name.
--
-- The difference between 'HasConstructor' and 'HasConstructor_' is similar to
-- the one between 'Data.Generics.Product.Fields.HasField' and
-- 'Data.Generics.Product.Fields.HasField_'.
-- See 'Data.Generics.Product.Fields.HasField_'.
class AsConstructor_ (ctor :: Symbol) s t a b where
  _Ctor_ :: Prism s t a b

class AsConstructor' (ctor :: Symbol) s a | ctor s -> a where
  _Ctor' :: Prism s s a a

-- |Sums that have a constructor with a given name.
--
-- This class gives the minimal constraints needed to define this prism.
-- For common uses, see 'HasConstructor'.
class AsConstructor0 (ctor :: Symbol) s t a b where
  _Ctor0 :: Prism s t a b

instance
  ( Generic s
  , ErrorUnless ctor s (HasCtorP ctor (Rep s))
  , GAsConstructor' ctor (Rep s) a
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic prism focusing on the "
                    ':<>: QuoteType ctor ':<>: 'Text " constructor of type " ':<>: QuoteType a
                  , 'Text "in " ':<>: QuoteType s])
    (() :: Constraint)
  ) => AsConstructor' ctor s a where
  _Ctor' eta = prismRavel (P.prismPRavel (P.repIso . _GCtor @ctor)) eta
  {-# INLINE[2] _Ctor' #-}

instance
  ( ErrorUnless ctor s (HasCtorP ctor (Rep s))
  , GAsConstructor' ctor (Rep s) a -- TODO: add a test similar to #62 for prisms
  , GAsConstructor' ctor (Rep (Indexed s)) a'
  , GAsConstructor ctor (Rep s) (Rep t) a b
  , t ~ Infer s a' b
  , GAsConstructor' ctor (Rep (Indexed t)) b'
  , s ~ Infer t b' a
  , AsConstructor0 ctor s t a b
  ) => AsConstructor ctor s t a b where

  _Ctor = _Ctor0 @ctor
  {-# INLINE[2] _Ctor #-}

-- | See Note [Uncluttering type signatures]
#if __GLASGOW_HASKELL__ < 804
-- >>> :t _Ctor
-- _Ctor
--   :: (Applicative f, Data.Profunctor.Choice.Choice p,
--       AsConstructor ctor s t a b) =>
--      p a (f b) -> p s (f t)
#else
-- >>> :t _Ctor
-- _Ctor
--   :: (AsConstructor ctor s t a b, Data.Profunctor.Choice.Choice p,
--       Applicative f) =>
--      p a (f b) -> p s (f t)
#endif
instance {-# OVERLAPPING #-} AsConstructor ctor (Void1 a) (Void1 b) a b where
  _Ctor = undefined

instance
  ( ErrorUnless ctor s (HasCtorP ctor (Rep s))
  , GAsConstructor' ctor (Rep s) a -- TODO: add a test similar to #62 for prisms
  , GAsConstructor' ctor (Rep (Indexed s)) a'
  , GAsConstructor ctor (Rep s) (Rep t) a b
  , GAsConstructor' ctor (Rep (Indexed t)) b'
  , UnifyHead s t
  , UnifyHead t s
  , AsConstructor0 ctor s t a b
  ) => AsConstructor_ ctor s t a b where

  _Ctor_ = _Ctor0 @ctor
  {-# INLINE[2] _Ctor_ #-}

instance {-# OVERLAPPING #-} AsConstructor_ ctor (Void1 a) (Void1 b) a b where
  _Ctor_ = undefined

instance
  ( Generic s
  , Generic t
  , GAsConstructor ctor (Rep s) (Rep t) a b
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic prism focusing on the "
                    ':<>: QuoteType ctor ':<>: 'Text " constructor of type " ':<>: QuoteType a
                  , 'Text "in " ':<>: QuoteType s])
    (() :: Constraint)
  ) => AsConstructor0 ctor s t a b where
  _Ctor0 = prismRavel (P.prismPRavel (P.repIso . _GCtor @ctor))
  {-# INLINE[2] _Ctor0 #-}

type family ErrorUnless (ctor :: Symbol) (s :: Type) (contains :: Bool) :: Constraint where
  ErrorUnless ctor s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a constructor named "
        ':<>: 'ShowType ctor
        )

  ErrorUnless _ _ 'True
    = ()
