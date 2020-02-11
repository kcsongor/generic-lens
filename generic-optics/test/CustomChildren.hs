{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CustomChildren
 ( customTypesTest
 ) where

import GHC.Generics
import Data.Generics.Product
import Test.HUnit
import Optics.Core
import Data.Kind

-- Opaque has no Generic instance
data Opaque = Opaque String
  deriving (Show, Eq)

-- Hide does have a Generic instance, but we want to hide its contents
-- from the traversal
data Hide = Hide String
  deriving (Show, Generic, Eq)

-- We first define a symbol for the custom traversal
data Custom

type instance Children Custom a = ChildrenCustom a

type family ChildrenCustom (a :: Type) where
  ChildrenCustom Opaque = '[String] -- here we state explicitly that Opaque contains a String
  ChildrenCustom Hide = '[] -- and hide the contents of Hide
  ChildrenCustom a = Children ChGeneric a -- for the rest, we defer to the generic children

-- We define the traversal of Opaque like so:
instance HasTypesCustom Custom Opaque Opaque String String where
  typesCustom f (Opaque str) = Opaque <$> f str

customTypesTest1 :: Test
customTypesTest1
  = TestCase (assertEqual "foo" (over (typesUsing @Custom @String) (++ "!") original) expected)
  where original = (Opaque "foo", Hide "bar")
        expected = (Opaque "foo!", Hide "bar") -- only Opaque's String gets modified

customTypesTest2 :: Test
customTypesTest2
  = TestCase (assertEqual "foo" (over (typesUsing @Custom @String) (++ "!") original) expected)
  where original = Opaque "foo"
        expected = Opaque "foo!"

customTypesTest3 :: Test
customTypesTest3
  = TestCase (assertEqual "foo" (over (typesUsing @Custom @String) (++ "!") original) expected)
  where original = Hide "foo"
        expected = Hide "foo"

customTypesTest :: Test
customTypesTest = TestList [customTypesTest1, customTypesTest2, customTypesTest3]
