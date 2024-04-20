-- | Parser types for defining combinators and such.

module Data.Type.Symbol.Parser.Types where

import GHC.TypeLits
import DeFun.Core ( type (~>) )

-- | Parse a 'Char' with the given state.
--
-- The action is always consuming. For this reason, you may need to look ahead
-- for the final case, so as to not consume an extra 'Char'. This prevents many
-- zero-length parsers. It's a bit weird. See
-- 'Data.Type.Symbol.Parser.Combinator.Drop' for an example.
type ParserCh  s r = Char -> s -> Result s r

-- | The result of a single step of a parser.
data Result s r
  = Cont s -- ^ OK, continue with the given state
  | Done r -- ^ OK, parse successful with result @r@
  | Err  E -- ^ parse error

-- | What a parser should do at the end of a 'Symbol'.
type ParserEnd s r = s -> Either E r

-- | A parser you can pass (heh) around.
--
-- Parsers are defined by the product of a 'ParserCh' character parser,
-- 'ParserEnd' end handler, and 's' starting state.
type Parser s r = (ParserChSym s r, ParserEndSym s r, s)

-- | A defunctionalization symbol for a 'ParserCh'.
type ParserChSym s r = Char ~> s ~> Result s r

-- | A defunctionalization symbol for a 'ParserEnd'.
type ParserEndSym s r = s ~> Either E r

-- | Parser error.
data E
  -- | Base parser error.
  = EBase
        Symbol       -- ^ parser name
        ErrorMessage -- ^ error message

  -- | Inner parser error inside combinator.
  | EIn
        Symbol -- ^ combinator name
        E      -- ^ inner error

-- | Error while running parser.
data ERun
  -- | Parser error at index X, character C.
  = ERun Natural Char E

  -- | Parser error on the empty string.
  | ERun0 E
