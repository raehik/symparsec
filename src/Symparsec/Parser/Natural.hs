{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Natural
  ( type NatBase, type NatBase1
  , type NatDec
  , type NatHex
  , type NatBin
  , type NatOct

  , type NatBaseWhile
  ) where

import Symparsec.Parser.Common
import Symparsec.Parser.Natural.Digits

-- TODO decide which version to export primarily, isolate or while

{-
The main loops here consume greedily to hopefully speed up evaluation.
It adds to the complexity, and means we have to backtrack on failure.
It'd be nice to assert that it /does/ help in some way.
-}

-- | Parse a binary (base 2) 'Natural'.
type NatBin = NatBase  2 ParseDigitBinSym

-- | Parse an octal (base 8) 'Natural'.
type NatOct = NatBase  8 ParseDigitOctSym

-- | Parse a decimal (base 10) 'Natural'.
type NatDec = NatBase 10 ParseDigitDecSym

-- | Parse a hexadecimal (base 16) 'Natural'. Permits mixed-case (@0-9A-Fa-f@).
type NatHex = NatBase 16 ParseDigitHexSym

-- | Parse a non-empty 'Natural' using the given base and digit parser.
--
-- Only permits parsing numbers with digits exactly one 'Char' long.
--
-- Returns an error if it parses zero digits, or if any character fails to
-- parse.
type NatBase :: Natural -> (Char ~> Maybe Natural) -> PParser Natural
data NatBase base parseDigit s
type instance App (NatBase base parseDigit) s =
    NatBaseStart base parseDigit s (UnconsState s)
type family NatBaseStart base parseDigit sCh s where
    NatBaseStart base parseDigit sCh '(Just ch, s) =
        NatBaseLoop base parseDigit sCh s 0 ch (parseDigit @@ ch) (UnconsState s)
    NatBaseStart base parseDigit sCh '(Nothing, s) = 'Reply (Err EEmpty) sCh

-- | Parse a 'Natural' with the given starting value.
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
type EInvalidDigit ch base =
    Error1 ( "not a base " ++ ShowNatDec base ++ " digit: " ++ ShowChar ch)

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

-- | Parse a non-empty 'Natural' using the given base and digit parser.
--
-- Only permits parsing numbers with digits exactly one 'Char' long.
--
-- Returns an error if it parses zero digits, or if the first digit fails to
-- parse. Returns success on parsing up to EOF, or just before the first failed
-- character parse. (Should match the behaviour of Megaparsec's number parsers.)
type NatBaseWhile :: Natural -> (Char ~> Maybe Natural) -> PParser Natural
data NatBaseWhile base parseDigit s
type instance App (NatBaseWhile base parseDigit) s =
    NatBaseWhileStart base parseDigit s (UnconsState s)
type family NatBaseWhileStart base parseDigit sCh s where
    NatBaseWhileStart base parseDigit sCh '(Just ch, s) =
        NatBaseWhileStart2 base parseDigit sCh s ch (parseDigit @@ ch) (UnconsState s)
    NatBaseWhileStart base parseDigit sCh '(Nothing, s) = 'Reply (Err EEmpty) sCh

-- TODO While1

type family NatBaseWhileStart2 base parseDigit sCh s chChur mDigit ms where
    NatBaseWhileStart2 base parseDigit sCh s chCur (Just digit) '(Just ch, sNext) =
        NatBaseWhileLoop base parseDigit s sNext digit ch (parseDigit @@ ch) (UnconsState sNext)
    NatBaseWhileStart2 base parseDigit sCh s chCur (Just digit) '(Nothing, sNext) =
        -- parsed first digit, no more input: done
        'Reply (OK digit) sNext
    NatBaseWhileStart2 base parseDigit sCh s chCur Nothing      _ =
        -- failed to parse first digit: backtrack and error
        'Reply (Err (EInvalidDigit chCur base)) sCh

-- Note that this parser never fails.
type NatBaseWhileLoop
    :: Natural
    -> (Char ~> Maybe Natural)
    -> PState
    -> PState
    -> Natural
    -> Char
    -> Maybe Natural
    -> (Maybe Char, PState)
    -> PReply Natural
type family NatBaseWhileLoop base parseDigit sCh s n chCur mDigit ms where
    -- parsed digit and have next char
    NatBaseWhileLoop base parseDigit sCh s n chCur (Just digit) '(Just ch, sNext) =
        NatBaseWhileLoop base parseDigit s sNext (n * base + digit) ch (parseDigit @@ ch) (UnconsState sNext)
    NatBaseWhileLoop base parseDigit sCh s n chCur (Just digit) '(Nothing, sNext) =
        'Reply (OK (n * base + digit)) sNext
    NatBaseWhileLoop base parseDigit sCh s n chCur Nothing      _ =
        -- failed to parse next digit: backtrack and finish
        'Reply (OK n) sCh
