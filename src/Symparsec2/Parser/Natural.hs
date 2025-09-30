{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Natural where

import Symparsec2.Parser.Common
import GHC.TypeNats ( type Natural, type (+), type (*) )
import DeFun.Core ( type (~>), type (@@) )
import TypeLevelShow.Natural ( type ShowNatDec )
import TypeLevelShow.Utils ( type ShowChar )

type NatBase :: Natural -> (Char ~> Maybe Natural) -> PParserSym Natural
data NatBase base parseDigit s
type instance App (NatBase base parseDigit) s =
    NatBaseStart base parseDigit (UnconsState s)
type family NatBaseStart base parseDigit s where
    NatBaseStart base parseDigit '(Just ch, s) =
        NatBaseLoop s base parseDigit 0 ch (parseDigit @@ ch) (UnconsState s)
    NatBaseStart base parseDigit '(Nothing, s) = Err s EEmpty

-- | Parse a non-empty 'Natural'.
--
-- Skips some extra work. May be handy for hand-written parsers.
type NatBase1 :: Natural -> (Char ~> Maybe Natural) -> Natural -> PParserSym Natural
data NatBase1 base parseDigit digit s
type instance App (NatBase1 base parseDigit digit) s =
    NatBase1' s base parseDigit digit (UnconsState s)
type family NatBase1' sPrev base parseDigit digit s where
    NatBase1' sPrev base parseDigit digit '(Just ch, s) =
        NatBaseLoop s base parseDigit digit ch (parseDigit @@ ch) (UnconsState s)
    NatBase1' sPrev base parseDigit digit '(Nothing, s) =
        Done s digit

type EEmpty = EBase "NatBase" (Text "no digits parsed")
type EInvalidDigit ch base = EBase "NatBase"
    (Text "not a base " :<>: Text (ShowNatDec base) :<>: Text " digit: " :<>: Text (ShowChar ch))

-- consumes greedily to hopefully speed up evaluation-- means we have to
-- backtrack on failure
type NatBaseLoop
    :: PState
    -> Natural
    -> (Char ~> Maybe Natural)
    -> Natural
    -> Char
    -> Maybe Natural
    -> (Maybe Char, PState)
    -> PResult Natural
type family NatBaseLoop sPrev base parseDigit n chCur mDigit s where
    NatBaseLoop sPrev base parseDigit n chCur (Just digit) '(Just ch, s) =
        NatBaseLoop s base parseDigit (n * base + digit) ch (parseDigit @@ ch) (UnconsState s)
    NatBaseLoop sPrev base parseDigit n chCur (Just digit) '(Nothing, s) =
        Done s (n * base + digit)
    NatBaseLoop sPrev base parseDigit n chCur Nothing      '(Just ch, s) =
        -- we've consumed the next character, but digit parse failed:
        -- backtrack and return error
        Err  sPrev (EInvalidDigit chCur base)
    NatBaseLoop sPrev base parseDigit n chCur Nothing      '(Nothing, s) =
        Err  s (EInvalidDigit chCur base)
