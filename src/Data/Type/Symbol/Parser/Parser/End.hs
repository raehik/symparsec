module Data.Type.Symbol.Parser.Parser.End ( End ) where

import Data.Type.Symbol.Parser.Parser
import Data.Type.Symbol.Parser.Parser.Common ( FailChSym, EmitEndSym )
import GHC.TypeLits
import DeFun.Core ( type (~>), type App )

-- | Assert end of symbol, or fail.
type End :: Parser () ()
type End = '(FailChSym "End" (Text "expected end of symbol"), EmitEndSym, '())
