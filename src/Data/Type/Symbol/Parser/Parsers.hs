-- | Re-exported type-level symbol parsers.
--
-- You may ignore the equations that Haddock displays: they are internal and
-- irrelevant to library usage.

module Data.Type.Symbol.Parser.Parsers
  (
  -- * Binary combinators
  -- $binary-combinators
    (:<*>:)
  ,  (:*>:)
  , (:<*:)
  , (:<|>:)

  -- * Positional
  -- $positional
  , Take
  , Skip
  , End
  , Isolate

  -- * Basic
  -- $basic
  , Literal

  -- ** Naturals
  , NatDec
  , NatHex
  , NatBin
  , NatOct
  , NatBase
  ) where

import Data.Type.Symbol.Parser.Parser.Isolate
import Data.Type.Symbol.Parser.Parser.Skip
import Data.Type.Symbol.Parser.Parser.Natural
import Data.Type.Symbol.Parser.Parser.Then
import Data.Type.Symbol.Parser.Parser.Then.VoidLeft
import Data.Type.Symbol.Parser.Parser.Then.VoidRight
import Data.Type.Symbol.Parser.Parser.Literal
import Data.Type.Symbol.Parser.Parser.End
import Data.Type.Symbol.Parser.Parser.Take
import Data.Type.Symbol.Parser.Parser.Or

-- $binary-combinators
-- Parsers that combine two parsers. Any parsers that have term-level parallels
-- will use the same fixity e.g. ':<*>:' is @infixl 4@, same as '<*>'.

-- $positional
-- Parsers that relate to symbol position e.g. length, end of symbol.

-- $basic
-- Simple non-combinator parsers. Probably fundamental in some way e.g. very
-- general or common.
