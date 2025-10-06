{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Skip ( type Skip, type SkipUnsafe ) where

import Symparsec2.Parser.Common
import Symparsec2.Parser.Ensure
import Symparsec2.Parser.Applicative
import Data.Type.Symbol qualified as Symbol

-- | Skip forward @n@ characters. Fails if fewer than @n@ characters remain.
type Skip :: Natural -> PParserSym ()
type Skip n = Ensure n *> SkipUnsafe n

-- | Skip forward @n@ characters. If fewer than @n@ characters remain, drops
--   entire input and succeeds.
type SkipUnsafe :: Natural -> PParserSym ()
data SkipUnsafe n s
type instance App (SkipUnsafe n) s = SkipUnsafe' n s
type family SkipUnsafe' n s where
    SkipUnsafe' n ('State rem len idx) =
        'Reply (OK '()) ('State (Symbol.Drop n rem) (len-n) (idx+n))

{-
type Skip :: Natural -> PParserSym ()
data Skip n s
type instance App (Skip n) s = Skip' n s (UnconsState s)
type family Skip' n sPrev s where
    Skip' 0 sPrev _              = 'Reply (OK '()) sPrev
    Skip' n sPrev '(Just _ch, s) = Skip' (n-1) s (UnconsState s)
    Skip' n sPrev '(Nothing,  s) = 'Reply (Err (Error1 (EStrInputTooShort n 0))) sPrev

type ESkipPastEnd n =
    (      Text "tried to skip "
      :<>: Text (ShowNatDec n) :<>: Text " chars from empty string")
-}
