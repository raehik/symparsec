{- | Parse digits from type-level 'Char's.

A 'Nothing' indicates the given 'Char' was not a valid digit for the given base.
-}

module Symparsec.Parser.Natural.Digits where

import GHC.TypeLits
import DeFun.Core
import Singleraeh.Equality ( testEqElse )
import Singleraeh.Maybe
import Unsafe.Coerce ( unsafeCoerce )

type SParseDigit parseDigit = Lam SChar (SMaybe SNat) parseDigit

class SingParseDigit parseDigit where
    singParseDigit :: SParseDigit parseDigit

-- | Parse a binary digit (0 or 1).
type family ParseDigitBin (ch :: Char) :: Maybe Natural where
    ParseDigitBin '0' = Just 0
    ParseDigitBin '1' = Just 1
    ParseDigitBin _   = Nothing

type ParseDigitBinSym :: Char ~> Maybe Natural
data ParseDigitBinSym ch
type instance App ParseDigitBinSym ch = ParseDigitBin ch

sParseDigitBinSym :: SParseDigit ParseDigitBinSym
sParseDigitBinSym = Lam $ \ch ->
      testEqElse ch (SChar @'0') (SJust (SNat @0))
    $ testEqElse ch (SChar @'1') (SJust (SNat @1))
    $ unsafeCoerce SNothing

instance SingParseDigit ParseDigitBinSym where
    singParseDigit = sParseDigitBinSym

-- | Parse an octal digit (0-7).
type family ParseDigitOct (ch :: Char) :: Maybe Natural where
    ParseDigitOct '0' = Just 0
    ParseDigitOct '1' = Just 1
    ParseDigitOct '2' = Just 2
    ParseDigitOct '3' = Just 3
    ParseDigitOct '4' = Just 4
    ParseDigitOct '5' = Just 5
    ParseDigitOct '6' = Just 6
    ParseDigitOct '7' = Just 7
    ParseDigitOct _   = Nothing

type ParseDigitOctSym :: Char ~> Maybe Natural
data ParseDigitOctSym ch
type instance App ParseDigitOctSym ch = ParseDigitOct ch

sParseDigitOctSym :: SParseDigit ParseDigitOctSym
sParseDigitOctSym = Lam $ \ch ->
      testEqElse ch (SChar @'0') (SJust (SNat @0))
    $ testEqElse ch (SChar @'1') (SJust (SNat @1))
    $ testEqElse ch (SChar @'2') (SJust (SNat @2))
    $ testEqElse ch (SChar @'3') (SJust (SNat @3))
    $ testEqElse ch (SChar @'4') (SJust (SNat @4))
    $ testEqElse ch (SChar @'5') (SJust (SNat @5))
    $ testEqElse ch (SChar @'6') (SJust (SNat @6))
    $ testEqElse ch (SChar @'7') (SJust (SNat @7))
    $ unsafeCoerce SNothing

instance SingParseDigit ParseDigitOctSym where
    singParseDigit = sParseDigitOctSym

-- | Parse a decimal digit (0-9).
type family ParseDigitDec (ch :: Char) :: Maybe Natural where
    ParseDigitDec '0' = Just 0
    ParseDigitDec '1' = Just 1
    ParseDigitDec '2' = Just 2
    ParseDigitDec '3' = Just 3
    ParseDigitDec '4' = Just 4
    ParseDigitDec '5' = Just 5
    ParseDigitDec '6' = Just 6
    ParseDigitDec '7' = Just 7
    ParseDigitDec '8' = Just 8
    ParseDigitDec '9' = Just 9
    ParseDigitDec _   = Nothing

type ParseDigitDecSym :: Char ~> Maybe Natural
data ParseDigitDecSym ch
type instance App ParseDigitDecSym ch = ParseDigitDec ch

sParseDigitDecSym :: SParseDigit ParseDigitDecSym
sParseDigitDecSym = Lam $ \ch ->
      testEqElse ch (SChar @'0') (SJust (SNat @0))
    $ testEqElse ch (SChar @'1') (SJust (SNat @1))
    $ testEqElse ch (SChar @'2') (SJust (SNat @2))
    $ testEqElse ch (SChar @'3') (SJust (SNat @3))
    $ testEqElse ch (SChar @'4') (SJust (SNat @4))
    $ testEqElse ch (SChar @'5') (SJust (SNat @5))
    $ testEqElse ch (SChar @'6') (SJust (SNat @6))
    $ testEqElse ch (SChar @'7') (SJust (SNat @7))
    $ testEqElse ch (SChar @'8') (SJust (SNat @8))
    $ testEqElse ch (SChar @'9') (SJust (SNat @9))
    $ unsafeCoerce SNothing

instance SingParseDigit ParseDigitDecSym where
    singParseDigit = sParseDigitDecSym

-- | Parse a hexadecimal digit (0-9A-Fa-f).
--
-- Both upper and lower case are permitted.
type family ParseDigitHex (ch :: Char) :: Maybe Natural where
    ParseDigitHex '0' = Just  0
    ParseDigitHex '1' = Just  1
    ParseDigitHex '2' = Just  2
    ParseDigitHex '3' = Just  3
    ParseDigitHex '4' = Just  4
    ParseDigitHex '5' = Just  5
    ParseDigitHex '6' = Just  6
    ParseDigitHex '7' = Just  7
    ParseDigitHex '8' = Just  8
    ParseDigitHex '9' = Just  9
    ParseDigitHex 'a' = Just 10
    ParseDigitHex 'A' = Just 10
    ParseDigitHex 'b' = Just 11
    ParseDigitHex 'B' = Just 11
    ParseDigitHex 'c' = Just 12
    ParseDigitHex 'C' = Just 12
    ParseDigitHex 'd' = Just 13
    ParseDigitHex 'D' = Just 13
    ParseDigitHex 'e' = Just 14
    ParseDigitHex 'E' = Just 14
    ParseDigitHex 'f' = Just 15
    ParseDigitHex 'F' = Just 15
    ParseDigitHex _   = Nothing

type ParseDigitHexSym :: Char ~> Maybe Natural
data ParseDigitHexSym ch
type instance App ParseDigitHexSym ch = ParseDigitHex ch

sParseDigitHexSym :: SParseDigit ParseDigitHexSym
sParseDigitHexSym = Lam $ \ch ->
      testEqElse ch (SChar @'0') (SJust (SNat  @0))
    $ testEqElse ch (SChar @'1') (SJust (SNat  @1))
    $ testEqElse ch (SChar @'2') (SJust (SNat  @2))
    $ testEqElse ch (SChar @'3') (SJust (SNat  @3))
    $ testEqElse ch (SChar @'4') (SJust (SNat  @4))
    $ testEqElse ch (SChar @'5') (SJust (SNat  @5))
    $ testEqElse ch (SChar @'6') (SJust (SNat  @6))
    $ testEqElse ch (SChar @'7') (SJust (SNat  @7))
    $ testEqElse ch (SChar @'8') (SJust (SNat  @8))
    $ testEqElse ch (SChar @'9') (SJust (SNat  @9))
    $ testEqElse ch (SChar @'a') (SJust (SNat @10))
    $ testEqElse ch (SChar @'A') (SJust (SNat @10))
    $ testEqElse ch (SChar @'b') (SJust (SNat @11))
    $ testEqElse ch (SChar @'B') (SJust (SNat @11))
    $ testEqElse ch (SChar @'c') (SJust (SNat @12))
    $ testEqElse ch (SChar @'C') (SJust (SNat @12))
    $ testEqElse ch (SChar @'d') (SJust (SNat @13))
    $ testEqElse ch (SChar @'D') (SJust (SNat @13))
    $ testEqElse ch (SChar @'e') (SJust (SNat @14))
    $ testEqElse ch (SChar @'E') (SJust (SNat @14))
    $ testEqElse ch (SChar @'f') (SJust (SNat @15))
    $ testEqElse ch (SChar @'F') (SJust (SNat @15))
    $ unsafeCoerce SNothing

instance SingParseDigit ParseDigitHexSym where
    singParseDigit = sParseDigitHexSym
