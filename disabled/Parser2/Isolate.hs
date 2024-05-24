{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-} -- TODO for singling

-- TODO complex parser with weird edge cases. needs clean up & tests

module Symparsec.Parser.Isolate where -- ( Isolate ) where

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
type Isolate :: Natural -> Parser s0 s r -> Parser (Natural, s0) (Natural, s) r
type family Isolate n p where
    Isolate n ('Parser pCh pEnd s0 sInit) = Isolate' n pCh pEnd s0 sInit

-- unwrapped for instances
type Isolate' n pCh pEnd s0 sInit = 'Parser
    (IsolateChSym pCh pEnd)
    (IsolateEndSym pEnd)
    '(n, s0)
    (IsolateSInitSym sInit)

type SIsolateS ss = STuple2 SNat ss

sIsolate
    :: SNat n
    -> SParser ss0 ss sr p
    -> SParser (SIsolateS ss0) (SIsolateS ss) sr (Isolate n p)
sIsolate n (SParser pCh pEnd s0 sInit) = SParser
    (isolateChSym pCh pEnd)
    (isolateEndSym pEnd)
    (STuple2 n s0)
    (isolateSInitSym sInit)

instance
  ( p ~ 'Parser pCh pEnd s0 sInit
  , KnownNat n, SingParser p
  ) => SingParser (Isolate' n pCh pEnd s0 sInit) where
    type PS0 (Isolate' n pCh pEnd s0 sInit) =
        SIsolateS (PS0 ('Parser pCh pEnd s0 sInit))
    type PS (Isolate' n pCh pEnd s0 sInit) =
        SIsolateS (PS  ('Parser pCh pEnd s0 sInit))
    type PR (Isolate' n pCh pEnd s0 sInit) =
        PR ('Parser pCh pEnd s0 sInit)
    singParser' = sIsolate (natSing @n) (singParser @p)

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

type IsolateSInit
    :: ParserSInitSym s0 s
    -> ParserSInit (Natural, s0) (Natural, s)
type family IsolateSInit sInit s0 where
    IsolateSInit sInit '(0, s) = IsolateSInit0   (sInit @@ s)
    IsolateSInit sInit '(n, s) = IsolateSInitN n (sInit @@ s)

type family IsolateSInit0 ees where
    -- | isolating 0, wrapped parser is consuming: error
    IsolateSInit0 (Right      s)  = Left  '(EBase "Isolate"
        (Text "isolating 0 but wrapped parser consuming"), '(0, s))

    -- | isolating 0, wrapped parser is also non-consuming: emit
    IsolateSInit0 (Left  '(e, s)) = Left  '(e, '(0, s))

type family IsolateSInitN n ees where
    -- | isolating n, wrapped parser is consuming: OK
    IsolateSInitN n (Right      s)  = Right '(n, s)

    -- | isolating n, wrapped parser is non-consuming: error
    --
    -- TODO should we be wrapping here? my intuition is that the end handler
    -- will do its job
    IsolateSInitN n (Left  '(e, s)) = Left  '(e, '(n, s))

isolateSInitN
    :: SNat n
    -> SResultSInit ss ees
    -> SResultSInit (SIsolateS ss) (IsolateSInitN n ees)
isolateSInitN n = \case
  SRight s             -> SRight $ STuple2 n s
  SLeft  (STuple2 e s) -> SLeft  $ STuple2 e $ STuple2 n s

type IsolateSInitSym
    :: ParserSInitSym s0 s
    -> ParserSInitSym (Natural, s0) (Natural, s)
data IsolateSInitSym sInit s
type instance App (IsolateSInitSym sInit) s = IsolateSInit sInit s

isolateSInitSym
    :: SParserSInitSym ss0 ss sInit
    -> SParserSInitSym (SIsolateS ss0) (SIsolateS ss) (IsolateSInitSym sInit)
isolateSInitSym sInit = Lam $ \(STuple2 n s0) ->
    case testEquality n (SNat @0) of
      Just Refl ->
        case sInit @@ s0 of
          SRight s             -> SLeft $ STuple2 singE $ STuple2 natSing s
          SLeft  (STuple2 e s) -> SLeft $ STuple2 e     $ STuple2 natSing s
      Nothing   -> unsafeCoerce $ isolateSInitN n (sInit @@ s0)
