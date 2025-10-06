-- | Common type-level string parsers.
--
-- Many parsers reuse term-level names, which can cause ambiguity issues.
-- Consider importing qualified.

module Symparsec2.Parsers
  (
  -- * Type class-esque
  -- $type-classes
    type (<$>)
  , type (<*>), type Pure, type LiftA2, type (*>), type (<*)
  , type (>>=)
  , type (<|>), type Empty

  -- * Positional
  -- $positional
  , type Ensure
  , type Isolate
  , type Take
  , type TakeRest
  , type Skip
  , type End

  -- * Other combinators
  -- $comb-etc
  , type Try
  , type While
  , type Count

  -- * Common non-combinator
  -- $noncomb-common
  , type Literal

  -- ** Naturals
  , type NatBase
  , type NatDec
  , type NatHex
  , type NatBin
  , type NatOct

  -- * Missing parsers
  -- $missing
  ) where

import Symparsec2.Parser.Alternative
import Symparsec2.Parser.Applicative
import Symparsec2.Parser.Count
import Symparsec2.Parser.End
import Symparsec2.Parser.Ensure
import Symparsec2.Parser.Functor
import Symparsec2.Parser.Isolate
import Symparsec2.Parser.Literal
import Symparsec2.Parser.Monad
import Symparsec2.Parser.Natural
import Symparsec2.Parser.Skip
import Symparsec2.Parser.Take
import Symparsec2.Parser.TakeRest
import Symparsec2.Parser.Try
import Symparsec2.Parser.While

{- $type-classes
Parsers which mirror functions from type classes (specifically 'Functor',
'Applicative', 'Monad' and 'Alternative'. These primitive combinators are
powerful, but can be tough to use without type-level binders or do-notation, and
force interacting with defunctionalization.
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
Certain term-level parsers you may be used to you will _not_ see in Symparsec:

* Parsers that rely on underlying instances
  * e.g. Semigroup, Monoid rely on underlying `Semigroup a` instances, which we
    would have to pass manually, which defeats the purpose (e.g. `(<>) = liftA2
    (<>)`, so just use like `LiftA2 (++)`
-}
