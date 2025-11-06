{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Natural
  ( type NatBase, type NatBase1, type NatBase1Sym
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
type NatBase :: Natural -> (Char ~> Maybe Natural) -> PParser s Natural
data NatBase base parseDigit ps
type instance App (NatBase base parseDigit) ps =
    NatBaseStart base parseDigit ps (UnconsState ps)
type family NatBaseStart base parseDigit psCh ps where
    NatBaseStart base parseDigit psCh '(Just ch, ps) =
        NatBaseLoop base parseDigit psCh ps 0 ch (parseDigit @@ ch) (UnconsState ps)
    NatBaseStart base parseDigit psCh '(Nothing, ps) = 'Reply (Err EEmpty) psCh

-- | Parse a 'Natural' with the given starting value.
--
-- Skips some extra work. May be handy for hand-written parsers.
type NatBase1 :: Natural -> (Char ~> Maybe Natural) -> Natural -> PParser s Natural
data NatBase1 base parseDigit digit ps
type instance App (NatBase1 base parseDigit digit) ps =
    NatBase1' base parseDigit ps digit (UnconsState ps)
type family NatBase1' base parseDigit psCh digit ps where
    NatBase1' base parseDigit psCh digit '(Just ch, ps) =
        NatBaseLoop base parseDigit psCh ps digit ch (parseDigit @@ ch) (UnconsState ps)
    NatBase1' base parseDigit psCh digit '(Nothing, ps) =
        'Reply (OK digit) ps

type EEmpty = Error1 "no digits parsed" -- TODO not great eh
type EInvalidDigit ch base =
    Error1 ( "not a base " ++ ShowNatDec base ++ " digit: " ++ ShowChar ch)

type NatBaseLoop
    :: Natural
    -> (Char ~> Maybe Natural)
    -> PState s
    -> PState s
    -> Natural
    -> Char
    -> Maybe Natural
    -> (Maybe Char, PState s)
    -> PReply s Natural
type family NatBaseLoop base parseDigit psCh ps n chCur mDigit mps where
    -- parsed digit and have next char
    NatBaseLoop base parseDigit psCh ps n chCur (Just digit) '(Just ch, psNext) =
        NatBaseLoop base parseDigit ps psNext (n * base + digit) ch (parseDigit @@ ch) (UnconsState psNext)
    NatBaseLoop base parseDigit psCh ps n chCur (Just digit) '(Nothing, psNext) =
        'Reply (OK (n * base + digit)) psNext
    NatBaseLoop base parseDigit psCh ps n chCur Nothing      '(_, psNext) =
        -- we've consumed the next character, but digit parse failed:
        -- backtrack and return error
        'Reply (Err (EInvalidDigit chCur base)) psCh

-- | Parse a non-empty 'Natural' using the given base and digit parser.
--
-- Only permits parsing numbers with digits exactly one 'Char' long.
--
-- Returns an error if it parses zero digits, or if the first digit fails to
-- parse. Returns success on parsing up to EOF, or just before the first failed
-- character parse. (Should match the behaviour of Megaparsec's number parsers.)
type NatBaseWhile :: Natural -> (Char ~> Maybe Natural) -> PParser s Natural
data NatBaseWhile base parseDigit ps
type instance App (NatBaseWhile base parseDigit) ps =
    NatBaseWhileStart base parseDigit ps (UnconsState ps)
type family NatBaseWhileStart base parseDigit psCh mps where
    NatBaseWhileStart base parseDigit psCh '(Just ch, ps) =
        NatBaseWhileStart2 base parseDigit psCh ps ch (parseDigit @@ ch) (UnconsState ps)
    NatBaseWhileStart base parseDigit psCh '(Nothing, ps) = 'Reply (Err EEmpty) psCh

-- TODO While1

type family NatBaseWhileStart2 base parseDigit psCh ps chChur mDigit mps where
    NatBaseWhileStart2 base parseDigit psCh ps chCur (Just digit) '(Just ch, psNext) =
        NatBaseWhileLoop base parseDigit ps psNext digit ch (parseDigit @@ ch) (UnconsState psNext)
    NatBaseWhileStart2 base parseDigit psCh ps chCur (Just digit) '(Nothing, psNext) =
        -- parsed first digit, no more input: done
        'Reply (OK digit) psNext
    NatBaseWhileStart2 base parseDigit psCh ps chCur Nothing      _ =
        -- failed to parse first digit: backtrack and error
        'Reply (Err (EInvalidDigit chCur base)) psCh

-- Note that this parser never fails.
type NatBaseWhileLoop
    :: Natural
    -> (Char ~> Maybe Natural)
    -> PState s
    -> PState s
    -> Natural
    -> Char
    -> Maybe Natural
    -> (Maybe Char, PState s)
    -> PReply s Natural
type family NatBaseWhileLoop base parseDigit psCh ps n chCur mDigit mps where
    -- parsed digit and have next char
    NatBaseWhileLoop base parseDigit psCh ps n chCur (Just digit) '(Just ch, psNext) =
        NatBaseWhileLoop base parseDigit ps psNext (n * base + digit) ch (parseDigit @@ ch) (UnconsState psNext)
    NatBaseWhileLoop base parseDigit psCh ps n chCur (Just digit) '(Nothing, psNext) =
        'Reply (OK (n * base + digit)) psNext
    NatBaseWhileLoop base parseDigit psCh ps n chCur Nothing      _ =
        -- failed to parse next digit: backtrack and finish
        'Reply (OK n) psCh

type NatBase1Sym :: Natural -> (Char ~> Maybe Natural) -> Natural ~> PParser s Natural
data NatBase1Sym base parseDigit x
type instance App (NatBase1Sym base parseDigit) x = NatBase1 base parseDigit x
