module Symparsec.Parser.End ( End ) where

import Symparsec.Parser.Common

-- | Assert end of symbol, or fail.
type End :: ParserSym () ()
type End = 'ParserSym
    (FailChSym "End" (Text "expected end of symbol")) RightSym '()
