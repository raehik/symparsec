{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Natural
  ( type NatBase, type NatBase1
  , type NatDec
  , type NatHex
  , type NatBin
  , type NatOct
  ) where

import Symparsec2.Parser.Common
import Symparsec2.Parser.Natural.Digits

-- | Parse a binary (base 2) 'Natural'.
type NatBin = NatBase  2 ParseDigitBinSym

-- | Parse an octal (base 8) 'Natural'.
type NatOct = NatBase  8 ParseDigitOctSym

-- | Parse a decimal (base 10) 'Natural'.
type NatDec = NatBase 10 ParseDigitDecSym

-- | Parse a hexadecimal (base 16) 'Natural'. Permits mixed-case (@0-9A-Fa-f@).
type NatHex = NatBase 16 ParseDigitHexSym

type NatBase :: Natural -> (Char ~> Maybe Natural) -> PParser Natural
data NatBase base parseDigit s
type instance App (NatBase base parseDigit) s =
    NatBaseStart base parseDigit s (UnconsState s)
type family NatBaseStart base parseDigit sCh s where
    NatBaseStart base parseDigit sCh '(Just ch, s) =
        NatBaseLoop base parseDigit sCh s 0 ch (parseDigit @@ ch) (UnconsState s)
    NatBaseStart base parseDigit sCh '(Nothing, s) = 'Reply (Err EEmpty) sCh

-- | Parse a non-empty 'Natural'.
--
-- Skips some extra work. May be handy for hand-written parsers.
type NatBase1 :: Natural -> (Char ~> Maybe Natural) -> Natural -> PParser Natural
data NatBase1 base parseDigit digit s
type instance App (NatBase1 base parseDigit digit) s =
    NatBase1' base parseDigit s digit (UnconsState s)
type family NatBase1' base parseDigit sCh digit s where
    NatBase1' base parseDigit sCh digit '(Just ch, s) =
        NatBaseLoop base parseDigit sCh s digit ch (parseDigit @@ ch) (UnconsState s)
    NatBase1' base parseDigit sCh digit '(Nothing, s) =
        'Reply (OK digit) s

type EEmpty = Error1 "no digits parsed" -- TODO not great eh
--type EInvalidDigit ch base = EBase "NatBase"
--    (Text "not a base " :<>: Text (ShowNatDec base) :<>: Text " digit: " :<>: Text (ShowChar ch))
type EInvalidDigit ch base =
    Error1 ( "not a base " ++ ShowNatDec base ++ " digit: " ++ ShowChar ch)

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
    -> PReply Natural
type family NatBaseLoop base parseDigit sCh s n chCur mDigit ms where
    -- parsed digit and have next char
    NatBaseLoop base parseDigit sCh s n chCur (Just digit) '(Just ch, sNext) =
        NatBaseLoop base parseDigit s sNext (n * base + digit) ch (parseDigit @@ ch) (UnconsState sNext)
    NatBaseLoop base parseDigit sCh s n chCur (Just digit) '(Nothing, sNext) =
        'Reply (OK (n * base + digit)) sNext
    NatBaseLoop base parseDigit sCh s n chCur Nothing      '(_, sNext) =
        -- we've consumed the next character, but digit parse failed:
        -- backtrack and return error
        'Reply (Err (EInvalidDigit chCur base)) sCh
