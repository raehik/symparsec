{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Skip where

import Symparsec2.Parser.Common

-- | Skip forward @n@ characters. Fails if fewer than @n@ characters are
--   available.
type Skip :: Natural -> PParserSym ()
data Skip n s
type instance App (Skip n) s = Skip' n s (UnconsState s)

type family Skip' n sPrev s where
    Skip' 0 sPrev _              = Done sPrev '()
    Skip' n sPrev '(Just _ch, s) = Skip' (n-1) s (UnconsState s)
    Skip' n sPrev '(Nothing,  s) = Err s (ESkipPastEnd n)

type ESkipPastEnd n = EBase "Skip"
    (      Text "tried to skip "
      :<>: Text (ShowNatDec n) :<>: Text " chars from empty string")
