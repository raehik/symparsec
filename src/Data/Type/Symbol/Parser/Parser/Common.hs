-- | Common definitions for parsers.

module Data.Type.Symbol.Parser.Parser.Common
  ( FailChSym
  , FailEndSym
  , EmitEndSym
  , ErrParserLimitation
  ) where

import Data.Type.Symbol.Parser.Parser
import GHC.TypeLits
import DeFun.Core ( type App, type (~>) )

-- | Fail with the given message when given any character to parse.
type FailChSym :: Symbol -> ErrorMessage -> ParserChSym s r
data FailChSym name e f
type instance App (FailChSym name e) f = FailChSym1 name e f

type FailChSym1 :: Symbol -> ErrorMessage -> Char -> s ~> Result s r
data FailChSym1 name e ch s
type instance App (FailChSym1 name e ch) s = Err (EBase name e)

-- | Fail with the given message if we're at the end of the symbol.
type FailEndSym :: Symbol -> ErrorMessage -> ParserEndSym s r
data FailEndSym name e s
type instance App (FailEndSym name e) s = Left (EBase name e)

-- | At the end of the symbol, emit parser state.
type EmitEndSym :: ParserEndSym r r
data EmitEndSym r
type instance App EmitEndSym r = Right r

-- | Helper for writing error messages to do with parser limitations (e.g. if
--   you tried to use a non-consuming parser like @Skip 0@).
type ErrParserLimitation :: Symbol -> ErrorMessage
type ErrParserLimitation msg = Text "parser limitation: " :<>: Text msg
