-- | Common definitions for parsers.

module Symparsec.Parser.Common
  (
  -- * Re-exports
    module Symparsec.Parser
  , Doc(..)
  , type (~>), type (@@), type App

  -- * Common definitions
  , FailChSym,  failChSym
  , FailEndSym, failEndSym
  , ErrParserLimitation
  ) where

import Symparsec.Parser
import GHC.TypeLits hiding ( ErrorMessage(..) )
import DeFun.Core
import TypeLevelShow.Doc
import Singleraeh.Either

-- | Fail with the given message when given any character to parse.
type FailCh name e = Err (EBase name e)

type FailChSym :: Symbol -> PDoc -> ParserChSym s r
data FailChSym name e f
type instance App (FailChSym name e) f = FailChSym1 name e f

type FailChSym1 :: Symbol -> PDoc -> ParserChSym1 s r
data FailChSym1 name e ch s
type instance App (FailChSym1 name e ch) s = FailCh name e

failChSym
    :: SSymbol name -> SDoc e -> SParserChSym ss sr (FailChSym name e)
failChSym name e = Lam2 $ \_ch _s -> SErr $ SEBase name e

-- | Fail with the given message if we're at the end of the symbol.
type FailEndSym :: Symbol -> PDoc -> ParserEndSym s r
data FailEndSym name e s
type instance App (FailEndSym name e) s = Left (EBase name e)

failEndSym
    :: SSymbol name -> SDoc e -> SParserEndSym ss sr (FailEndSym name e)
failEndSym name e = Lam $ \_s -> SLeft $ SEBase name e

-- | Helper for writing error messages to do with parser limitations (e.g. if
--   you tried to use a non-consuming parser like @Skip 0@).
type ErrParserLimitation :: Symbol -> PDoc
type ErrParserLimitation msg = Text "parser limitation: " :<>: Text msg
