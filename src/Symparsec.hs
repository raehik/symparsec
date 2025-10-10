{- | Top-level Symparsec module, exporting builtin parsers and runners.

I suggest importing this module qualified. Or, consider the following imports:

@
import "Symparsec.Run" qualified as Symparsec
import "Symparsec.Parsers" qualified as P
-- > :k! Symparsec.Run (P.Take 1) "hello"
@
-}

module Symparsec
  (
  -- * Base definitions
    type Run
  , type RunTest

  -- * Parsers
  , module Symparsec.Parsers
  ) where

import Symparsec.Run
import Symparsec.Parsers
