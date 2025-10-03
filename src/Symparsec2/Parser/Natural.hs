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
    NatBaseStart base parseDigit s (UnconsState s)
type family NatBaseStart base parseDigit sCh s where
    NatBaseStart base parseDigit sCh '(Just ch, s) =
        NatBaseLoop base parseDigit sCh s 0 ch (parseDigit @@ ch) (UnconsState s)
    NatBaseStart base parseDigit sCh '(Nothing, s) = Err s EEmpty

-- | Parse a non-empty 'Natural'.
--
-- Skips some extra work. May be handy for hand-written parsers.
type NatBase1 :: Natural -> (Char ~> Maybe Natural) -> Natural -> PParserSym Natural
data NatBase1 base parseDigit digit s
type instance App (NatBase1 base parseDigit digit) s =
    NatBase1' base parseDigit s digit (UnconsState s)
type family NatBase1' base parseDigit sCh digit s where
    NatBase1' base parseDigit sCh digit '(Just ch, s) =
        NatBaseLoop base parseDigit sCh s digit ch (parseDigit @@ ch) (UnconsState s)
    NatBase1' base parseDigit sCh digit '(Nothing, s) =
        Done s digit

type EEmpty = EBase "NatBase" (Text "no digits parsed")
type EInvalidDigit ch base = EBase "NatBase"
    (Text "not a base " :<>: Text (ShowNatDec base) :<>: Text " digit: " :<>: Text (ShowChar ch))

-- consumes greedily to hopefully speed up evaluation-- means we have to
-- backtrack on failure
-- oof, but it really adds to the complexity, I got state parsing for errors
-- wrong originally. TODO?
type NatBaseLoop
    :: Natural
    -> (Char ~> Maybe Natural)
    -> PState
    -> PState
    -> Natural
    -> Char
    -> Maybe Natural
    -> (Maybe Char, PState)
    -> PResult Natural
type family NatBaseLoop base parseDigit sCh s n chCur mDigit ms where
    -- parsed digit and have next char
    NatBaseLoop base parseDigit sCh s n chCur (Just digit) '(Just ch, sNext) =
        NatBaseLoop base parseDigit s sNext (n * base + digit) ch (parseDigit @@ ch) (UnconsState sNext)
    NatBaseLoop base parseDigit sCh s n chCur (Just digit) '(Nothing, sNext) =
        Done sNext (n * base + digit)
    NatBaseLoop base parseDigit sCh s n chCur Nothing      '(Just ch, sNext) =
        -- we've consumed the next character, but digit parse failed:
        -- backtrack and return error
        Err  sCh (EInvalidDigit chCur base)
    NatBaseLoop base parseDigit sCh s n chCur Nothing      '(Nothing, sNext) =
        Err  sCh (EInvalidDigit chCur base)
