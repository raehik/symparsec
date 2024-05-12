module Symparsec.Parser.End ( End ) where

import Symparsec.Parser ( Parser )
import Symparsec.Parser.Common ( FailChSym, EmitEndSym )
import GHC.TypeLits ( ErrorMessage(Text) )

-- | Assert end of symbol, or fail.
type End :: Parser () ()
type End = '(FailChSym "End" (Text "expected end of symbol"), EmitEndSym, '())
