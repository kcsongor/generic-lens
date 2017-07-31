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
-- Derive record field getters and setters generically.
--
-----------------------------------------------------------------------------

module Data.Generics.Product.Typed
  ( -- *Lenses
    --
    --  $example
    HasType (..)

    -- *Internals
  , GHasType (..)
  ) where

import Data.Generics.Internal.Families
import Data.Generics.Internal.Lens

import Data.Kind
import GHC.Generics
import GHC.TypeLits

--  $example
--  @
--    module Example where
--
--    import Data.Generics.Product
--    import GHC.Generics
--
--    data Human = Human
--      { name    :: String
--      , age     :: Int
--      , address :: String
--      }
--      deriving (Generic, Show)
--
--    human :: Human
--    human = Human \"Tunyasz\" 50 \"London\"
--  @

--  | Records that have a field with a unique type.
class HasType a s where
  -- |A lens that focuses on a field with a unique type in its parent type.
  --  Compatible with the lens package's 'Control.Lens.Lens' type.
  --
  --  >>> human ^. typed @Int
  --  50
  typed :: Lens' s a
  typed f t
    = fmap (flip (setTyped @a) t) (f (getTyped @a t))

  -- |Get field at type
  getTyped :: s -> a
  getTyped s = s ^. typed @a

  -- |Set field at type
  setTyped :: a -> s -> s
  setTyped = set (typed @a)

  {-# MINIMAL typed | setTyped, getTyped #-}

instance
  ( Generic s
  , ErrorUnlessOne a s (CountTotalType a (Rep s))
  , GHasType (Rep s) a
  ) => HasType a s where

  typed = repIso . gtyped

type family ErrorUnlessOne (a :: Type) (s :: Type) (count :: Count) :: Constraint where
  ErrorUnlessOne a s 'None
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a value of type "
        ':<>: 'ShowType a
        )

  ErrorUnlessOne a s 'Multiple
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " contains multiple values of type "
        ':<>: 'ShowType a
        ':<>: 'Text "; the choice of value is thus ambiguous"
        )

  ErrorUnlessOne _ _ 'One
    = ()

-- |As 'HasType' but over generic representations as defined by
--  "GHC.Generics".
class GHasType (f :: Type -> Type) a where
  gtyped :: Lens' (f x) a

instance GProductHasType l r a (HasTotalTypeP a l)
      => GHasType (l :*: r) a where

  gtyped = gproductTyped @_ @_ @_ @(HasTotalTypeP a l)

instance (GHasType l a, GHasType r a) => GHasType (l :+: r) a where
  gtyped = combine (gtyped @l) (gtyped @r)

instance GHasType (K1 R a) a where
  gtyped f (K1 x) = fmap K1 (f x)

instance GHasType f a => GHasType (M1 m meta f) a where
  gtyped = mIso . gtyped

class GProductHasType l r a (contains :: Bool) where
  gproductTyped :: Lens' ((l :*: r) x) a

instance GHasType l a => GProductHasType l r a 'True where
  gproductTyped = first . gtyped

instance GHasType r a => GProductHasType l r a 'False where
  gproductTyped = second . gtyped
