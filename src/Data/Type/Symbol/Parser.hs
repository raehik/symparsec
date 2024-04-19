module Data.Type.Symbol.Parser
  (
  -- * Base definitions
    Parser
  , RunParser

  -- * Parsers
  -- ** Combinators
  , Isolate
  , (:<*>:)
  ,  (:*>:)
  , (:<*:)

  -- ** Primitives
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

import Data.Type.Symbol.Parser.Internal
import Data.Type.Symbol.Parser.Isolate
import Data.Type.Symbol.Parser.Drop
import Data.Type.Symbol.Parser.Natural
import Data.Type.Symbol.Parser.Then
import Data.Type.Symbol.Parser.Then.VoidLeft
import Data.Type.Symbol.Parser.Then.VoidRight
import Data.Type.Symbol.Parser.Literal
import Data.Type.Symbol.Parser.End

-- | Sequence parsers, returning both values in a tuple.
type pl :<*>: pr = Then   pl pr

-- | Sequence parsers, discarding the return value of the left parser
type pl  :*>: pr = ThenVL pl pr

-- | Sequence parsers, discarding the return value of the right parser.
--
-- Consider using ':*>:' instead, which is simpler and potentially faster since
-- we parse L->R.
type pl :<*:  pr = ThenVR pl pr
