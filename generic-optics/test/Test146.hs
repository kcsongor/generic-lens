{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Test146 where

import Control.Monad.Except
import Data.Generics.Sum
import GHC.Generics

data Error = Error
  deriving (Generic)

poly :: (AsType Error e, MonadError e m) => m ()
poly = undefined

mono :: ExceptT Error IO ()
mono = poly



