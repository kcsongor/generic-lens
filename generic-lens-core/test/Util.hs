module Util where

import Test.Inspection
import Test.HUnit.Base

mkHUnitTest :: Result -> Test
mkHUnitTest r = TestCase $
  case r of
    Success _s -> return ()
    Failure s -> assertFailure s

