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

type pl :<*>: pr = Then   pl pr
type pl  :*>: pr = ThenVL pl pr
type pl :<*:  pr = ThenVR pl pr
