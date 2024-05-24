{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Take where

import Symparsec.Parser.Common
import Singleraeh.Symbol ( RevCharsToSymbol, revCharsToSymbol )
import Singleraeh.List ( SList(..) )
import Singleraeh.Tuple ( STuple2(..) )
import Data.Type.Equality
import Singleraeh.Natural ( (%-) )
import Singleraeh.Either ( SEither(..) )
import TypeLevelShow.Natural
import GHC.TypeLits hiding ( ErrorMessage(..) )
import Unsafe.Coerce ( unsafeCoerce )
import DeFun.Core

-- | Return the next @n@ characters.
type Take n = 'PParser TakeChSym TakeEndSym '(n, '[])

type STakeS = STuple2 SNat (SList SChar)
type  TakeS = (Natural, [Char])

sTake :: SNat n -> SParser STakeS SSymbol (Take n)
sTake n = SParser takeChSym takeEndSym (STuple2 n SNil)

instance KnownNat n => SingParser (Take n) where
    type PS (Take n) = STakeS
    type PR (Take n) = SSymbol
    singParser' = sTake SNat

type TakeCh :: PParserCh TakeS Symbol
type family TakeCh ch s where
    TakeCh ch '(0, chs) = Done (RevCharsToSymbol chs)
    TakeCh ch '(n, chs) = Cont '(n-1, ch : chs)

type TakeChSym :: ParserChSym TakeS Symbol
data TakeChSym f
type instance App TakeChSym f = TakeChSym1 f

takeChSym :: SParserChSym STakeS SSymbol TakeChSym
takeChSym = Lam2 $ \ch (STuple2 n chs) ->
    case testEquality n (SNat @0) of
      Just Refl -> SDone $ revCharsToSymbol chs
      Nothing   ->
        unsafeCoerce $ SCont $ STuple2 (n %- (SNat @1)) (SCons ch chs)

type TakeChSym1 :: ParserChSym1 TakeS Symbol
data TakeChSym1 ch s
type instance App (TakeChSym1 ch) s = TakeCh ch s

type TakeEnd :: PParserEnd TakeS Symbol
type family TakeEnd s where
    TakeEnd '(0, chs) = Right (RevCharsToSymbol chs)
    TakeEnd '(n, _)   = Left (ETakeEnd n)

type ETakeEnd n = EBase "Take"
    (      Text "tried to take "
      :<>: Text (ShowNatDec n) :<>: Text " chars from empty string")

eTakeEnd :: SNat n -> SE (ETakeEnd n)
eTakeEnd sn = withKnownSymbol (sShowNatDec sn) singE

type TakeEndSym :: ParserEndSym TakeS Symbol
data TakeEndSym s
type instance App TakeEndSym s = TakeEnd s

takeEndSym :: SParserEndSym STakeS SSymbol TakeEndSym
takeEndSym = Lam $ \(STuple2 n chs) ->
    case testEquality n (SNat @0) of
      Just Refl -> SRight $ revCharsToSymbol chs
      Nothing   -> unsafeCoerce $ SLeft $ eTakeEnd n
