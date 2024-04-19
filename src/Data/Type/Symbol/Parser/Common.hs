module Data.Type.Symbol.Parser.Common
  ( FailChSym
  , FailEndSym
  , EmitEndSym
  , ErrParserLimitation
  ) where

import Data.Type.Symbol.Parser.Types
import GHC.TypeLits
import DeFun.Core ( type App, type (~>) )

type FailChSym :: Symbol -> ErrorMessage -> ParserChSym s r
data FailChSym name e f
type instance App (FailChSym name e) f = FailChSym1 name e f

type FailChSym1 :: Symbol -> ErrorMessage -> Char -> s ~> Result s r
data FailChSym1 name e ch s
type instance App (FailChSym1 name e ch) s = Err (EBase name e)

type FailEndSym :: Symbol -> ErrorMessage -> ParserEndSym s r
data FailEndSym name e s
type instance App (FailEndSym name e) s = Left (EBase name e)

-- | Emit state directly on end of input.
type EmitEndSym :: ParserEndSym r r
data EmitEndSym r
type instance App EmitEndSym r = Right r

type ErrParserLimitation :: Symbol -> ErrorMessage
type ErrParserLimitation msg = Text "parser limitation: " :<>: Text msg
