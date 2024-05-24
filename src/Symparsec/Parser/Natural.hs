{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Natural where

import Symparsec.Parser.Common
import Symparsec.Parser.Natural.Digits
import DeFun.Core
import GHC.TypeLits hiding ( ErrorMessage(..) )
import TypeLevelShow.Natural ( ShowNatDec, sShowNatDec )
import Singleraeh.Maybe
import Singleraeh.Either
import Singleraeh.Natural

-- | Parse a binary (base 2) natural.
type NatBin = NatBase  2 ParseDigitBinSym

-- | Parse an octal (base 8) natural.
type NatOct = NatBase  8 ParseDigitOctSym

-- | Parse a decimal (base 10) natural.
type NatDec = NatBase 10 ParseDigitDecSym

-- | Parse a hexadecimal (base 16) natural. Permits mixed-case (@0-9A-Fa-f@).
type NatHex = NatBase 16 ParseDigitHexSym

-- | Parse a natural in the given base, using the given digit parser.
type NatBase
    :: Natural -> (Char ~> Maybe Natural) -> PParser (Maybe Natural) Natural
type NatBase base parseDigit =
    'PParser (NatBaseChSym base parseDigit) NatBaseEndSym Nothing

sNatBase
    :: SNat base
    -> SParseDigit parseDigit
    -> SParser (SMaybe SNat) SNat (NatBase base parseDigit)
sNatBase base parseDigit =
    SParser (sNatBaseChSym base parseDigit) sNatBaseEndSym SNothing

instance (KnownNat base, SingParseDigit parseDigit)
  => SingParser (NatBase base parseDigit) where
    type PS (NatBase base parseDigit) = SMaybe SNat
    type PR (NatBase base parseDigit) = SNat
    singParser' = sNatBase natSing singParseDigit

type NatBaseCh
    :: Natural
    -> (Char ~> Maybe Natural)
    -> PParserCh (Maybe Natural) Natural
type NatBaseCh base parseDigit ch mn = NatBaseCh' base mn (parseDigit @@ ch)

type family NatBaseCh' base mn mDigit where
    NatBaseCh' base (Just n) (Just digit) = Cont (Just (n * base + digit))
    NatBaseCh' base Nothing  (Just digit) = Cont (Just digit)
    NatBaseCh' base mn       Nothing      = Err (EInvalidDigit base)

type EInvalidDigit base = EBase "NatBase"
    (Text "not a base " :<>: Text (ShowNatDec base) :<>: Text " digit")
eInvalidDigit :: SNat base -> SE (EInvalidDigit base)
eInvalidDigit base = withKnownSymbol (sShowNatDec base) singE

type NatBaseChSym
    :: Natural
    -> (Char ~> Maybe Natural)
    -> ParserChSym (Maybe Natural) Natural
data NatBaseChSym base parseDigit f
type instance App (NatBaseChSym base parseDigit) f =
    NatBaseChSym1 base parseDigit f

type NatBaseChSym1
    :: Natural
    -> (Char ~> Maybe Natural)
    -> ParserChSym1 (Maybe Natural) Natural
data NatBaseChSym1 base parseDigit ch mn
type instance App (NatBaseChSym1 base parseDigit ch) mn =
    NatBaseCh base parseDigit ch mn

sNatBaseChSym
    :: SNat base
    -> SParseDigit parseDigit
    -> SParserChSym (SMaybe SNat) SNat (NatBaseChSym base parseDigit)
sNatBaseChSym base parseDigit = Lam2 $ \ch mn ->
    case parseDigit @@ ch of
      SJust digit ->
        case mn of
          SJust n  -> SCont $ SJust $ n %* base %+ digit
          SNothing -> SCont $ SJust digit
      SNothing -> SErr $ eInvalidDigit base

type family NatBaseEnd mn where
    NatBaseEnd (Just n) = Right n
    NatBaseEnd Nothing  = Left EEmpty

type EEmpty = EBase "NatBase" (Text "no digits parsed")
eEmpty :: SE EEmpty
eEmpty = singE

type NatBaseEndSym :: ParserEndSym (Maybe Natural) Natural
data NatBaseEndSym mn
type instance App NatBaseEndSym mn = NatBaseEnd mn

sNatBaseEndSym :: SParserEndSym (SMaybe SNat) SNat NatBaseEndSym
sNatBaseEndSym = Lam $ \case
  SJust n  -> SRight n
  SNothing -> SLeft eEmpty
