{-# LANGUAGE UndecidableInstances #-} -- for natural multiplication etc.

module Data.Type.Symbol.Parser.Natural where

import Data.Type.Symbol.Parser.Internal
import GHC.TypeLits
import DeFun.Core ( type (~>), type App, type (@@) )
import Data.Type.Char.Digits

type NatBin = NatBase  2 ParseBinaryDigitSym
type NatOct = NatBase  8 ParseOctalDigitSym
type NatDec = NatBase 10 ParseDecimalDigitSym
type NatHex = NatBase 16 ParseHexDigitSym

type NatBase
    :: Natural -> (Char ~> Maybe Natural) -> Parser Natural Natural
type NatBase base parseDigit =
    '(NatBaseChSym base parseDigit, NatBaseEndSym, 0)

type NatBaseCh
    :: Natural
    -> (Char ~> Maybe Natural)
    -> ParserCh Natural Natural
type family NatBaseCh base parseDigit ch n where
    NatBaseCh base parseDigit ch n =
        NatBaseCh' base n (parseDigit @@ ch)

type family NatBaseCh' base n mDigit where
    NatBaseCh' base n Nothing      =
        Err (Text "not a base " :<>: ShowType base :<>: Text " digit")
    NatBaseCh' base n (Just digit) = Cont (n * base + digit)

type NatBaseEnd :: ParserEnd Natural Natural
type NatBaseEnd n = Right n

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
