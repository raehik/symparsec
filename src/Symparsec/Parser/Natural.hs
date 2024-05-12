{-# LANGUAGE UndecidableInstances #-} -- for natural multiplication etc.

module Symparsec.Parser.Natural
  ( NatBin, NatOct, NatDec, NatHex
  , NatBase
  ) where

import Symparsec.Parser
import Symparsec.Parser.Common ( EmitEndSym )
import GHC.TypeLits
import DeFun.Core ( type (~>), type App, type (@@) )
import Symparsec.Internal.Digits

-- | Parse a binary (base 2) natural.
type NatBin = NatBase  2 ParseBinaryDigitSym

-- | Parse an octal (base 8) natural.
type NatOct = NatBase  8 ParseOctalDigitSym

-- | Parse a decimal (base 10) natural.
type NatDec = NatBase 10 ParseDecimalDigitSym

-- | Parse a hexadecimal (base 16) natural. Permits mixed-case (@0-9A-Fa-f@).
type NatHex = NatBase 16 ParseHexDigitSym

-- | Parse a natural in the given base, using the given digit parser.
type NatBase
    :: Natural -> (Char ~> Maybe Natural) -> Parser Natural Natural
type NatBase base parseDigit =
    '(NatBaseChSym base parseDigit, EmitEndSym, 0)

type NatBaseCh
    :: Natural
    -> (Char ~> Maybe Natural)
    -> ParserCh Natural Natural
type NatBaseCh base parseDigit ch n = NatBaseCh' base n (parseDigit @@ ch)

type family NatBaseCh' base n mDigit where
    NatBaseCh' base n (Just digit) = Cont (n * base + digit)
    NatBaseCh' base n Nothing      = Err (EBase "NatBase"
        (Text "not a base " :<>: ShowType base :<>: Text " digit"))

type NatBaseChSym
    :: Natural
    -> (Char ~> Maybe Natural)
    -> ParserChSym Natural Natural
data NatBaseChSym base parseDigit f
type instance App (NatBaseChSym base parseDigit) f =
    NatBaseChSym1 base parseDigit f

type NatBaseChSym1
    :: Natural
    -> (Char ~> Maybe Natural)
    -> Char -> Natural
    ~> Result Natural Natural
data NatBaseChSym1 base parseDigit ch n
type instance App (NatBaseChSym1 base parseDigit ch) n =
    NatBaseCh base parseDigit ch n

type ParseBinaryDigitSym :: Char ~> Maybe Natural
data ParseBinaryDigitSym a
type instance App ParseBinaryDigitSym a = ParseBinaryDigit a

type ParseOctalDigitSym :: Char ~> Maybe Natural
data ParseOctalDigitSym a
type instance App ParseOctalDigitSym a = ParseOctalDigit a

type ParseDecimalDigitSym :: Char ~> Maybe Natural
data ParseDecimalDigitSym a
type instance App ParseDecimalDigitSym a = ParseDecimalDigit a

type ParseHexDigitSym :: Char ~> Maybe Natural
data ParseHexDigitSym a
type instance App ParseHexDigitSym a = ParseHexDigit a
