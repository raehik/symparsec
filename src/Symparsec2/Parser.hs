{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser where

import DeFun.Core
import GHC.TypeLits ( type Symbol )
import GHC.TypeNats ( type Natural )
import TypeLevelShow.Doc

data State str n = State
  -- | Remaining input.
  { remaining :: str

  -- | Input length.
  --
  -- Permitted to be less than actual remaining input. This is fine.
  -- It means parsers can act on a substring of the input.
  , length :: n

  -- | Index in the input string.
  --
  -- Should be "overall" index, and only for errors.
  , index :: n
  }

-- | Promoted 'State', for type-level use.
type PState = State Symbol Natural

-- | TODO
data E str
  = EBase
        str
        (Doc str)
  | EIn
        str
        (E str)
    deriving stock Show

-- | Promoted 'E'.
type PE = E Symbol

-- | Parser result.
data Result str n r = Result
  -- | Parsed value, or failure.
  { result :: Either (E str) r

  -- | Final parser state.
  , state :: State str n
  }

-- | Promoted 'Result'.
type PResult = Result Symbol Natural

type  Parser str n r = State str n -> Result str n r
type PParser       r = Parser Symbol Natural r
type  ParserSym str n r = State str n ~> Result str n r
type PParserSym       r = ParserSym Symbol Natural r
