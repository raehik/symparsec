module Data.Type.Symbol.Parser
  (
  -- * Base definitions
    Parser
  , Run

  -- * Parsers
  -- ** Combinators
  , Isolate
  , (:<*>:)
  ,  (:*>:)
  , (:<*:)

  -- ** Primitives
  , Take
  , Drop
  , Literal
  , End

  -- *** Naturals
  , NatDec
  , NatHex
  , NatBin
  , NatOct
  , NatBase
  ) where

import Data.Type.Symbol.Parser.Run
import Data.Type.Symbol.Parser.Types
import Data.Type.Symbol.Parser.Combinator.Isolate
import Data.Type.Symbol.Parser.Combinator.Drop
import Data.Type.Symbol.Parser.Combinator.Natural
import Data.Type.Symbol.Parser.Combinator.Then
import Data.Type.Symbol.Parser.Combinator.Then.VoidLeft
import Data.Type.Symbol.Parser.Combinator.Then.VoidRight
import Data.Type.Symbol.Parser.Combinator.Literal
import Data.Type.Symbol.Parser.Combinator.End
import Data.Type.Symbol.Parser.Combinator.Take

-- | Sequence parsers, returning both values in a tuple.
type pl :<*>: pr = Then   pl pr

-- | Sequence parsers, discarding the return value of the left parser
type pl  :*>: pr = ThenVL pl pr

-- | Sequence parsers, discarding the return value of the right parser.
--
-- Consider using ':*>:' instead, which is simpler and potentially faster since
-- we parse L->R.
type pl :<*:  pr = ThenVR pl pr
