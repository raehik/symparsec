-- | Common type-level string parsers.
--
-- Many parsers reuse term-level names, which can cause ambiguity issues.
-- Consider importing qualified.

module Symparsec.Parsers
  (
  -- * Type class-esque
  -- $type-classes
    type (<$>)
  , type (<*>), type Pure, type LiftA2, type (*>), type (<*)
  , type (>>=)
  , type (<|>), type Empty, type Optional, type Many, type Some
  , type SepBy, type SepBy1
  , type Choice

  -- * Positional
  -- $positional
  , type Ensure
  , type Isolate
  , type Take
  , type Take1
  , type TakeRest
  , type Skip
  , type Eof

  -- * Other combinators
  -- $comb-etc
  , type Try
  , type While
  , type TakeWhile
  , type TakeWhile1
  , type Count
  , type Token
  , type Satisfy
  , type OneOf
  , type NoneOf

  -- * Common non-combinator
  -- $noncomb-common
  , type Literal

  -- ** Naturals
  , type NatBase, type NatBase1, type NatBase1Sym
  , type NatDec
  , type NatHex
  , type NatBin
  , type NatOct

  -- * Derived
  -- $derived
  , type Tuple

  -- * Missing parsers
  -- $missing
  ) where

import Symparsec.Parser.Alternative
import Symparsec.Parser.Applicative
import Symparsec.Parser.Count
import Symparsec.Parser.Ensure
import Symparsec.Parser.Eof
import Symparsec.Parser.Functor
import Symparsec.Parser.Isolate
import Symparsec.Parser.Literal
import Symparsec.Parser.Monad
import Symparsec.Parser.Natural
import Symparsec.Parser.Satisfy
import Symparsec.Parser.Skip
import Symparsec.Parser.Take
import Symparsec.Parser.TakeRest
import Symparsec.Parser.TakeWhile
import Symparsec.Parser.Token
import Symparsec.Parser.Try
import Symparsec.Parser.While
import DeFun.Core

{- $type-classes
Parsers which mirror functions from type classes (specifically 'Functor',
'Applicative', 'Monad' and 'Control.Alternative.Alternative'. These primitive
combinators are powerful, but can be tough to use without type-level binders or
do-notation, and force interacting with defunctionalization.
-}

{- $positional
Parsers that relate to input position e.g. length, end of input.
-}

{- $comb-etc
Assorted parser combinators (that wrap other parsers).
-}

{- $noncomb-common
Simple non-combinator parser. Probably fundamental in some way e.g. very general
or common.
-}

{- $missing
Certain term-level parsers you may be used to you will /not/ see in Symparsec:

* Parsers that rely on underlying instances e.g. no @'Semigroup' a => Semigroup
  (parser a)@ because we'd have to pass @Semigroup a@ manually, which defeats
  the purpose
-}

{- $derived
Derived parsers. Should be type synonyms.
-}

{- | Parse left, then right, and return their results in a tuple.

Classic parser combinators often don't define this because it's trivial, and do
notation is often cleaner anyway. But it's very syntactically busy on the type
level, and we don't have do notation. So here's a convenience definition.
-}
type Tuple l r = LiftA2 (Con2 '(,)) l r
