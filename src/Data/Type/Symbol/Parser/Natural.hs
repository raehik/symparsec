{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Natural where

import Data.Type.Symbol.Parser.Internal
import GHC.TypeLits
import DeFun.Core ( type (~>), type App, type (@@) )
import Data.Type.Char.Digits

type PNatBin = PNatBase  2 ParseBinaryDigitSym
type PNatOct = PNatBase  8 ParseOctalDigitSym
type PNatDec = PNatBase 10 ParseDecimalDigitSym
type PNatHex = PNatBase 16 ParseHexDigitSym

type PNatBase
    :: Natural -> (Char ~> Maybe Natural) -> ParserSym' Natural Natural
type PNatBase base parseDigit =
    '(NatBaseSym base parseDigit, NatBaseEndSym, 0)

type NatBase
    :: Natural
    -> (Char ~> Maybe Natural)
    -> Parser Natural Natural
type family NatBase base parseDigit ch n where
    NatBase base parseDigit ch n =
        NatBase' base n (parseDigit @@ ch)

type family NatBase' base n mDigit where
    NatBase' base n 'Nothing      =
        'Err ('Text "not a base " :<>: 'ShowType base :<>: 'Text " digit")
    NatBase' base n ('Just digit) = 'Cont (n * base + digit)

--type NatBaseEnd :: ParserEnd Natural Natural
type NatBaseEnd n = 'Right n

type NatBaseSym
    :: Natural
    -> (Char ~> Maybe Natural)
    -> ParserSym Natural Natural
data NatBaseSym base parseDigit f
type instance App (NatBaseSym base parseDigit) f =
    NatBaseSym1 base parseDigit f

type NatBaseSym1
    :: Natural
    -> (Char ~> Maybe Natural)
    -> Char -> Natural
    ~> Result Natural Natural
data NatBaseSym1 base parseDigit ch n
type instance App (NatBaseSym1 base parseDigit ch) n =
    NatBase base parseDigit ch n

type NatBaseEndSym :: ParserEndSym Natural Natural
data NatBaseEndSym n
type instance App NatBaseEndSym s = NatBaseEnd s

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

