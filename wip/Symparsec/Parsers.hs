-- | Type-level string parsers.
--
-- You may ignore the equations that Haddock displays: they are internal and
-- irrelevant to library usage.

module Symparsec.Parsers
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
  , TakeRest
  , Skip
  , End
  , Isolate

  -- * Predicated
  -- $predicated
  , While, TakeWhile

  -- * TODO unsorted
  , Count

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

import Symparsec.Parser.Isolate
import Symparsec.Parser.Skip
import Symparsec.Parser.Natural
import Symparsec.Parser.Then
import Symparsec.Parser.Then.VoidLeft
import Symparsec.Parser.Then.VoidRight
import Symparsec.Parser.Literal
import Symparsec.Parser.End
import Symparsec.Parser.Take
import Symparsec.Parser.TakeRest
import Symparsec.Parser.Or
import Symparsec.Parser.While
import Symparsec.Parser.Count

-- $binary-combinators
-- Parsers that combine two parsers. Any parsers that have term-level parallels
-- will use the same fixity e.g. ':<*>:' is @infixl 4@, same as '<*>'.

-- $positional
-- Parsers that relate to symbol position e.g. length, end of symbol.

-- $predicated
-- Parsers that include character predicates.

-- $basic
-- Simple non-combinator parsers. Probably fundamental in some way e.g. very
-- general or common.

type TakeWhile chPred = While chPred TakeRest
