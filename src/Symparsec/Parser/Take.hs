{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Symparsec.Parser.Take where -- ( Take, Take' ) where

import Symparsec.Parser.Common
import Singleraeh.Symbol ( RevCharsToSymbol, revCharsToSymbol )
import Singleraeh.List ( SList(..) )
import Singleraeh.Tuple ( STuple2(..) )
import Singleraeh.Equality ( testEqElse )
import Singleraeh.Natural ( (%-) )
import Singleraeh.Either ( SEither(..) )
import TypeLevelShow.Natural
import GHC.TypeLits hiding ( ErrorMessage(..) )
import Unsafe.Coerce ( unsafeCoerce )
import DeFun.Core

type SPTake n = SParser STakeS SSymbol (Take' n)
sTake :: SNat n -> SPTake n
sTake sn = SParser takeChSym takeEndSym (takeSInit sn)

instance KnownNat n => SingParser (Take' n) where
    type PS (Take' n) = STakeS
    type PR (Take' n) = SSymbol
    singParser' = sTake (SNat @n)

type TakeS = (Natural, [Char])
type STakeS = STuple2 SNat (SList SChar)

type TakeSInit n = '(n, '[])
takeSInit :: SNat n -> STakeS (TakeSInit n)
takeSInit sn = STuple2 sn SNil

-- | Return the next @n@ characters.
type Take :: Natural -> Parser TakeS Symbol
type family Take n where
    Take 0 = 'Parser
        (FailChSym "Take" (ErrParserLimitation "can't take 0"))
        TakeEndSym
        '(0, '[])
    Take n = Take' n

-- | Unsafe 'Take' which doesn't check for @n=0@. May get stuck.
type Take' n = 'Parser TakeChSym TakeEndSym (TakeSInit n)

type TakeCh :: PParserCh TakeS Symbol
type family TakeCh ch s where
    TakeCh ch '(1, chs) = Done (RevCharsToSymbol (ch : chs))
    TakeCh ch '(n, chs) = Cont '(n-1, ch : chs)

type TakeChSym :: ParserChSym (Natural, [Char]) Symbol
data TakeChSym f
type instance App TakeChSym f = TakeChSym1 f

takeChSym :: SParserChSym STakeS SSymbol TakeChSym
takeChSym = Lam2 $ \sch (STuple2 sn schs) ->
      testEqElse sn (SNat @1) (SDone $ revCharsToSymbol $ SCons sch schs)
    $ -- TODO need another unsafe coerce here again because of !1 -> n
      unsafeCoerce $ SCont $ STuple2 (sn %- (SNat @1)) (SCons sch schs)

type TakeChSym1 :: ParserChSym1 (Natural, [Char]) Symbol
data TakeChSym1 ch s
type instance App (TakeChSym1 ch) s = TakeCh ch s

type TakeEnd :: PParserEnd (Natural, [Char]) Symbol
type family TakeEnd s where
    TakeEnd '(0, chs) = Right (RevCharsToSymbol chs)
    TakeEnd '(n, _)   = Left (TakeEndE n)

type TakeEndE n = EBase "Take"
    (      Text "tried to take "
      :<>: Text (ShowNatDec n) :<>: Text " chars from empty string")

takeEndE :: SNat n -> SE (TakeEndE n)
takeEndE sn = withKnownSymbol (sShowNatDec sn) singE

type TakeEndSym :: ParserEndSym (Natural, [Char]) Symbol
data TakeEndSym s
type instance App TakeEndSym s = TakeEnd s

takeEndSym :: SParserEndSym STakeS SSymbol TakeEndSym
takeEndSym = Lam $ \(STuple2 sn schs) ->
      testEqElse sn (SNat @0) (SRight $ revCharsToSymbol schs)
    $ unsafeCoerce $ SLeft $ takeEndE sn
