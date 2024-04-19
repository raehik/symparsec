module Data.Type.Symbol.Parser.Common
  ( type FailChSym
  , type EmitEndSym
  ) where

import Data.Type.Symbol.Parser.Types
import GHC.TypeLits
import DeFun.Core ( type App, type (~>) )

type FailCh :: Symbol -> ParserCh s r
type family FailCh eStr ch s where
    FailCh eStr _ _ = Err (Text eStr)

type FailChSym :: Symbol -> ParserChSym s r
data FailChSym eStr f
type instance App (FailChSym eStr) f = FailChSym1 eStr f

type FailChSym1 :: Symbol -> Char -> s ~> Result s r
data FailChSym1 eStr ch s
type instance App (FailChSym1 eStr ch) s = FailCh eStr ch s

type EmitEnd :: ParserEnd r r
type EmitEnd r = Right r

type EmitEndSym :: ParserEndSym r r
data EmitEndSym r
type instance App EmitEndSym r = EmitEnd r
