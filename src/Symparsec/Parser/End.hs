module Symparsec.Parser.End ( End ) where

import Symparsec.Parser.Common

-- | Assert end of symbol, or fail.
type End :: Parser () ()
type End = 'Parser
    (FailChSym "End" (Text "expected end of symbol")) RightSym '()
