{- | Base definitions for Symparsec parsers.

Types are designed to be useable on both term and type level where possible.
Type synonyms are provided for promoted types, for saving keystrokes.

Some notes when writing parsers:

  * No need to insert the previous character into error messages, the runner
    does that.
-}

module Symparsec.Parser
  (
  -- * Parsers
    Parser(..)
  , ParserCh
  , ParserEnd
  , Result(..)
  , E(..)

  -- * Defun symbols
  , ParserSym(..)
  , ParserChSym
  , ParserChSym1
  , ParserEndSym

  -- * Promoted types
  , PParserCh
  , PParserEnd
  , PResult
  , PE
  ) where

import GHC.TypeLits
import DeFun.Core ( type (~>) )
import TypeLevelShow.Doc ( Doc )

-- | A term-level parser.
--
-- Parsers contain a character parser, an end handler, and an initial state.
--
-- May not be promoted due to containing function types.
data Parser str s r = Parser
  { parserCh   :: ParserCh  str s r
  -- ^ Character parser.
  , parserEnd  :: ParserEnd str s r
  -- ^ End handler.
  , parserInit :: s
  -- ^ Initial parser state.
  }

-- | A type-level parser, containing defunctionalization symbols.
data ParserSym s r = ParserSym
  { parserChSym   :: ParserChSym s r
  -- ^ A defunctionalization symbol for a character parser.
  , parserEndSym  :: ParserEndSym s r
  -- ^ A defunctionalization symbol for an end handler.
  , parserInit'   :: s
  -- ^ Initial parser state.
  }

-- | The result of a single step of a parser.
--
-- Promotable. Instantiate with 'String' for term-level, 'Symbol' for
-- type-level.
data Result str s r
  = Cont s       -- ^ OK, continue with the given state
  | Done r       -- ^ OK, parse successful with result @r@
  | Err  (E str) -- ^ parse error

-- | Parse a 'Char' with the given state.
--
-- The action is always consuming. For this reason, you may need to look ahead
-- for the final case, so as to not consume an extra 'Char'. This prevents many
-- zero-length parsers. It's a bit weird. See
-- 'Data.Type.Symbol.Parser.Parser.Drop' for an example.
type ParserCh str s r = Char -> s -> Result str s r

-- | A defunctionalization symbol for a 'ParserCh'.
type ParserChSym s r = Char ~> s ~> PResult s r

-- | A partially-applied defunctionalization symbol for a 'ParserCh'.
type ParserChSym1 s r = Char -> s ~> PResult s r

-- | What a parser should do at the end of a 'Symbol'.
type ParserEnd str s r = s -> Either (E str) r

-- | A defunctionalization symbol for a 'ParserEnd'.
type ParserEndSym s r = s ~> Either PE r

-- | Parser error.
--
-- Promotable. Instantiate with 'String' for term-level, 'Symbol' for
-- type-level.
data E str
  -- | Base parser error.
  = EBase
        str       -- ^ parser name
        (Doc str) -- ^ error message

  -- | Inner parser error inside combinator.
  | EIn
        str     -- ^ combinator name
        (E str) -- ^ inner error
    deriving stock Show

-- | Promoted 'ParserCh'.
type PParserCh s r = ParserCh Symbol s r

-- | Promoted 'ParserEnd'.
type PParserEnd s r = ParserEnd Symbol s r

-- | Promoted 'Result'.
type PResult = Result Symbol

-- | Promoted 'E'.
type PE = E Symbol
