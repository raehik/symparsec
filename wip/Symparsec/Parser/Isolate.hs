{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Isolate where

import Symparsec.Parser.Common
import GHC.TypeLits hiding ( ErrorMessage(..) )
import TypeLevelShow.Natural ( ShowNatDec, sShowNatDec )
import DeFun.Core
import Singleraeh.Either
import Singleraeh.Tuple
import Singleraeh.Equality ( testEqElse )
import Singleraeh.Natural ( (%-) )
import Unsafe.Coerce ( unsafeCoerce )
import Data.Type.Equality

-- | Run the given parser isolated to the next @n@ characters.
--
-- All isolated characters must be consumed.
type Isolate :: Natural -> PParser s r -> PParser (Natural, s) r
type family Isolate n p where
    Isolate n ('PParser pCh pEnd s0) = Isolate' n pCh pEnd s0

-- unwrapped for instances
type Isolate' n pCh pEnd s0 = 'PParser
    (IsolateChSym pCh pEnd)
    (IsolateEndSym pEnd)
    '(n, s0)

type SIsolateS ss = STuple2 SNat ss

sIsolate
    :: SNat n
    -> SParser ss sr p
    -> SParser (SIsolateS ss) sr (Isolate n p)
sIsolate n (SParser pCh pEnd s0) = SParser
    (sIsolateChSym pCh pEnd)
    (sIsolateEndSym pEnd)
    (STuple2 n s0)

instance
  ( p ~ 'PParser pCh pEnd s0
  , KnownNat n, SingParser p
  ) => SingParser (Isolate' n pCh pEnd s0) where
    type PS (Isolate' n pCh pEnd s0) =
        SIsolateS (PS ('PParser pCh pEnd s0))
    type PR (Isolate' n pCh pEnd s0) =
        PR ('PParser pCh pEnd s0)
    singParser' = sIsolate (natSing @n) (singParser @p)

type IsolateCh
    :: ParserChSym s r
    -> ParserEndSym s r
    -> PParserCh (Natural, s) r
type family IsolateCh pCh pEnd ch s where
    IsolateCh pCh pEnd ch '(0, s) = IsolateInnerEnd (pEnd @@ s)
    IsolateCh pCh pEnd ch '(n, s) = IsolateInner n (pCh @@ ch @@ s)

sIsolateChSym
    :: SParserChSym  ss sr pCh
    -> SParserEndSym ss sr pEnd
    -> SParserChSym (SIsolateS ss) sr (IsolateChSym pCh pEnd)
sIsolateChSym pCh pEnd = Lam2 $ \ch (STuple2 n s) ->
    case testEquality n (SNat @0) of
      Just Refl ->
        case pEnd @@ s of
          SRight r -> SDone r
          SLeft  e -> SErr $ eIsolateWrap e
      Nothing -> unsafeCoerce $ sIsolateInner n (pCh @@ ch @@ s)

type IsolateInnerEnd :: Either PE r -> PResult (Natural, s) r
type family IsolateInnerEnd a where
    IsolateInnerEnd (Right r) = Done r
    IsolateInnerEnd (Left  e) = Err  (EIsolateWrap e)

type IsolateInner :: Natural -> PResult s r -> PResult (Natural, s) r
type family IsolateInner n res where
    IsolateInner n (Cont s) = Cont '(n-1, s)
    IsolateInner n (Done _) = Err (EIsolateRemaining n)
    IsolateInner _ (Err  e) = Err (EIsolateWrap e)

sIsolateInner
    :: SNat n
    -> SResult ss sr res
    -> SResult (SIsolateS ss) sr (IsolateInner n res)
sIsolateInner n = \case
  SCont  s -> SCont $ STuple2 (n %- (SNat @1)) s
  SDone _r -> SErr  $ eIsolateRemaining n
  SErr   e -> SErr  $ eIsolateWrap e

type IsolateEnd :: ParserEndSym s r -> PParserEnd (Natural, s) r
type family IsolateEnd pEnd s where
    IsolateEnd pEnd '(0, s) = IsolateEnd' (pEnd @@ s)
    -- ^ will only occur on @Isolate 0@
    IsolateEnd pEnd '(n, s) = Left (EIsolateEndN n)

sIsolateEndSym
    :: SParserEndSym ss sr pEnd
    -> SParserEndSym (SIsolateS ss) sr (IsolateEndSym pEnd)
sIsolateEndSym pEnd = Lam $ \(STuple2 n s) ->
      testEqElse n (SNat @0) (sIsolateEnd' (pEnd @@ s))
    $ unsafeCoerce $ SLeft $ eIsolateEndN n

type IsolateEnd' :: Either PE r -> Either PE r
type family IsolateEnd' res where
    IsolateEnd' (Right r) = Right r
    IsolateEnd' (Left  e) = Left (EIsolateWrap e)

sIsolateEnd'
    :: SResultEnd sr res
    -> SResultEnd sr (IsolateEnd' res)
sIsolateEnd' = \case
  SRight r -> SRight r
  SLeft  e -> SLeft  $ eIsolateWrap e

type IsolateEndSym :: ParserEndSym s r -> ParserEndSym (Natural, s) r
data IsolateEndSym pEnd s
type instance App (IsolateEndSym pEnd) s = IsolateEnd pEnd s

type IsolateChSym
    :: ParserChSym s r
    -> ParserEndSym s r
    -> ParserChSym (Natural, s) r
data IsolateChSym pCh pEnd f
type instance App (IsolateChSym pCh pEnd) f = IsolateChSym1 pCh pEnd f

type IsolateChSym1
    :: ParserChSym s r
    -> ParserEndSym s r
    -> Char -> (Natural, s) ~> PResult (Natural, s) r
data IsolateChSym1 pCh pEnd ch s
type instance App (IsolateChSym1 pCh pEnd ch) s = IsolateCh pCh pEnd ch s

type EIsolateWrap e = EIn "Isolate" e

eIsolateWrap :: SE e -> SE (EIsolateWrap e)
eIsolateWrap e = withSingE e singE

type EIsolateEndN n = EBase "Isolate"
    (      Text "tried to isolate more than present (needed "
      :<>: Text (ShowNatDec n) :<>: Text " more)" )

eIsolateEndN :: SNat n -> SE (EIsolateEndN n)
eIsolateEndN n = withKnownSymbol (sShowNatDec n) singE

type EIsolateRemaining n = EBase "Isolate"
    (      Text "isolated parser ended without consuming all input ("
      :<>: Text (ShowNatDec n) :<>: Text " remaining)" )

eIsolateRemaining :: SNat n -> SE (EIsolateRemaining n)
eIsolateRemaining n = withKnownSymbol (sShowNatDec n) singE
