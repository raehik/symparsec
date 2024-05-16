-- | Common definitions for parsers.

module Symparsec.Parser.Common
  (
  -- * Re-exports
    module Symparsec.Parser
  , Doc(..)
  , type (~>), type (@@), type App

  -- * Common definitions
  , FailChSym
  , FailEndSym
  , RightSym
  , ErrParserLimitation
  ) where

import Symparsec.Parser
import GHC.TypeLits ( Symbol )
import DeFun.Core ( type App, type (~>), type (@@) )
import TypeLevelShow.Doc ( Doc(..), PDoc )

-- | Fail with the given message when given any character to parse.
type FailChSym :: Symbol -> PDoc -> ParserChSym s r
data FailChSym name e f
type instance App (FailChSym name e) f = FailChSym1 name e f

type FailChSym1 :: Symbol -> PDoc -> ParserChSym1 s r
data FailChSym1 name e ch s
type instance App (FailChSym1 name e ch) s = Err (EBase name e)

-- | Fail with the given message if we're at the end of the symbol.
type FailEndSym :: Symbol -> PDoc -> ParserEndSym s r
data FailEndSym name e s
type instance App (FailEndSym name e) s = Left (EBase name e)

-- | Defunctionalized promoted 'Right'.
--
-- May be used as an "emit parser state on end of string" end handler.
type RightSym :: b ~> Either a b
data RightSym b
type instance App RightSym b = Right b

-- | Helper for writing error messages to do with parser limitations (e.g. if
--   you tried to use a non-consuming parser like @Skip 0@).
type ErrParserLimitation :: Symbol -> PDoc
type ErrParserLimitation msg = Text "parser limitation: " :<>: Text msg
