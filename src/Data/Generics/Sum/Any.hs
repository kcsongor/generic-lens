{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Generics.Sum.Any
  ( AsAny (..)
  ) where

import Data.Generics.Internal.Lens
import Data.Generics.Sum.Constructors
import Data.Generics.Sum.Typed

class AsAny (sel :: k) a s | s sel k -> a where
  _As :: Prism' s a

instance AsConstructor ctor a s => AsAny ctor a s where
  _As = _Ctor @ctor

instance AsType a s => AsAny a a s where
  _As = _Typed @a
