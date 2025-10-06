{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser where

import DeFun.Core
import GHC.TypeLits ( type Symbol )
import GHC.TypeNats ( type Natural )

-- | Parser state.
data State str n = State
  -- | Remaining input.
  { remaining :: str

  -- | Remaining permitted length.
  --
  -- Must be less than or equal to the actual length of the remaining input.
  -- Parsers must use this field when reading from input:
  --
  -- * if ==0, treat as end of input.
  -- * if  >0 but remaining input is empty, unrecoverable parser error
  --
  -- This extra bookkeeping permits much simpler parser design, specifically for
  -- parsers that act on a substring of the input.
  , length :: n

  -- | Index in the input string.
  --
  -- Overall index. Used for nicer error reporting after parse completion.
  , index :: n
  }

-- | Promoted 'State'
type PState = State Symbol Natural

{-
data Span n = Span
  { start :: n
  , end   :: n
  } deriving stock Show
-}

data Error str = Error
  { detail :: [str]
  } deriving stock Show

-- | Promoted 'Error'.
type PError = Error Symbol

-- | Parser completion: result, and final state.
--
-- TODO: megaparsec also returns a bool indicating if any input was consumed.
-- Unsure what it's used for.
data Reply str n a = Reply
  { result :: Result str n a -- | Parse result.
  , state  :: State str n    -- | Final parser state.
  }

-- | Promoted 'Reply'.
type PReply = Reply Symbol Natural

-- | Parse result: a value, or an error.
data Result str n a = OK a            -- | Parser succeeded.
                    | Err (Error str) -- | Parser failed.

-- | Promoted 'Result'.
type PResult = Result Symbol Natural

-- | A parser is a function on parser state.
type Parser str n a = State str n -> Reply str n a

-- | Promoted 'Parser': a defunctionalization symbol to a function on promoted
--   parser state.
type PParser a = PState ~> PReply a
