{-# LANGUAGE UndecidableInstances #-}

-- TODO complex parser with weird edge cases. needs clean up & tests

module Symparsec.Parser.Isolate where -- ( Isolate, Isolate', Isolate'' ) where

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

type SIsolateS ss = STuple2 SNat ss

type SIsolate n ss sr pCh pEnd sInit =
    SParser (SIsolateS ss) sr (Isolate' n pCh pEnd sInit)

sIsolate
    :: SNat n
    -> SParser ss sr ('Parser pCh pEnd sInit)
    -> SIsolate n ss sr pCh pEnd sInit
sIsolate n (SParser pCh pEnd sInit) =
    SParser (isolateChSym pCh pEnd) (isolateEndSym pEnd) (STuple2 n sInit)

instance
  ( p ~ 'Parser pCh pEnd sInit
  , SingParser p
  , KnownNat n
  ) => SingParser (Isolate' n pCh pEnd sInit) where
    type PS (Isolate' n pCh pEnd sInit) =
        SIsolateS (PS ('Parser pCh pEnd sInit))
    type PR (Isolate' n pCh pEnd sInit) =
        PR ('Parser pCh pEnd sInit)
    singParser' = sIsolate (SNat @n) (singParser @p)

-- | Run the given parser isolated to the next @n@ characters.
--
-- All isolated characters must be consumed.
type Isolate :: Natural -> Parser s r -> Parser (Natural, s) r
type family Isolate n p where
    Isolate 0 ('Parser pCh pEnd s) = 'Parser
        (FailChSym "Isolate" (ErrParserLimitation "cannot isolate 0"))
        (IsolateEndSym pEnd) '(0, s)
    Isolate n ('Parser pCh pEnd s) = Isolate' n pCh pEnd s

-- | unsafe (doesn't check for bad stuck behaviour) and unwrapped for permitting
--   instances
type Isolate'
    :: Natural
    -> ParserChSym s r -> ParserEndSym s r -> s
    -> Parser (Natural, s) r
type Isolate' n pCh pEnd s =
    'Parser (IsolateChSym pCh pEnd) (IsolateEndSym pEnd) '(n, s)

type IsolateCh
    :: ParserChSym s r
    -> ParserEndSym s r
    -> ParserCh (Natural, s) r
type family IsolateCh pCh pEnd ch s where
    IsolateCh pCh pEnd ch '(1, s) = IsolateInnerEnd' pEnd (pCh @@ ch @@ s)
    IsolateCh pCh pEnd ch '(n, s) = IsolateInner n (pCh @@ ch @@ s)

isolateChSym
    :: SParserChSym  ss sr pCh
    -> SParserEndSym ss sr pEnd
    -> SParserChSym (SIsolateS ss) sr (IsolateChSym pCh pEnd)
isolateChSym pCh pEnd = Lam2 $ \ch (STuple2 n s) ->
    case testEquality n (SNat @1) of
      Just Refl ->
        case pCh @@ ch @@ s of
          SCont s' ->
            case pEnd @@ s' of
              SRight r -> SDone r
              SLeft  e -> SErr $ eIsolateWrap e
          SDone r -> SDone r
          SErr  e -> SErr $ eIsolateWrap e
      Nothing -> unsafeCoerce $ isolateInner n (pCh @@ ch @@ s)

-- TODO clean up names here

--type IsolateInnerEnd' :: Either ErrorMessage r -> Result (Natural, s) r
type family IsolateInnerEnd' pEnd res where
    IsolateInnerEnd' pEnd (Cont s) = IsolateInnerEnd (pEnd @@ s)
    IsolateInnerEnd' pEnd (Done r) = Done r
    IsolateInnerEnd' pEnd (Err  e) = Err  (EIsolateWrap e)

type IsolateInnerEnd :: Either PE r -> PResult (Natural, s) r
type family IsolateInnerEnd a where
    IsolateInnerEnd (Right r) = Done r
    IsolateInnerEnd (Left  e) = Err  (EIsolateWrap e)

type IsolateInner :: Natural -> PResult s r -> PResult (Natural, s) r
type family IsolateInner n res where
    IsolateInner n (Cont s) = Cont '(n-1, s)
    -- note the n-1 here, we have to do this since we're looking ahead
    IsolateInner n (Done _) = Err (EIsolateRemaining (n-1))
    IsolateInner _ (Err  e) = Err (EIsolateWrap e)

isolateInner
    :: SNat n
    -> SResult ss sr res
    -> SResult (SIsolateS ss) sr (IsolateInner n res)
isolateInner n = \case
  SCont  s -> SCont $ STuple2 (n %- (SNat @1)) s
  SDone _r -> SErr  $ eIsolateRemaining (n %- (SNat @1))
  SErr   e -> SErr  $ eIsolateWrap e

type IsolateEnd :: ParserEndSym s r -> ParserEnd (Natural, s) r
type family IsolateEnd pEnd s where
    IsolateEnd pEnd '(0, s) = IsolateEnd' (pEnd @@ s)
    -- ^ will only occur on @Isolate 0@
    IsolateEnd pEnd '(n, s) = Left (EIsolateEndN n)

isolateEndSym
    :: SParserEndSym ss sr pEnd
    -> SParserEndSym (SIsolateS ss) sr (IsolateEndSym pEnd)
isolateEndSym pEnd = Lam $ \(STuple2 n s) ->
      testEqElse n (SNat @0) (isolateEnd' (pEnd @@ s))
    $ unsafeCoerce $ SLeft $ eIsolateEndN n

type IsolateEnd' :: Either PE r -> Either PE r
type family IsolateEnd' res where
    IsolateEnd' (Right r) = Right r
    IsolateEnd' (Left  e) = Left (EIsolateWrap e)

isolateEnd'
    :: SResultEnd sr res
    -> SResultEnd sr (IsolateEnd' res)
isolateEnd' = \case
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
