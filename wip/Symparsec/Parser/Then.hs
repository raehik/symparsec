{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Then where

import Symparsec.Parser.Common
import Singleraeh.Either ( SEither(..) )
import Singleraeh.Tuple ( STuple2(..) )
import DeFun.Core

type SPThen ssl srl ssr srr plCh plEnd s0l prCh prEnd s0r =
    SParser
        (SEither ssl (STuple2 srl ssr))
        (STuple2 srl srr)
        (Then' plCh plEnd s0l prCh prEnd s0r)

sThen
    :: SParser ssl srl ('PParser plCh plEnd s0l)
    -> SParser ssr srr ('PParser prCh prEnd s0r)
    -> SPThen  ssl srl ssr srr plCh plEnd s0l prCh prEnd s0r
sThen (SParser plCh plEnd s0l) (SParser prCh prEnd s0r) =
    SParser (sThenChSym plCh prCh s0r) (sThenEndSym plEnd prEnd s0r) (SLeft s0l)

instance
  -- Shame I can't use pl, pr in associated type synonyms! :(
  ( pl ~ 'PParser plCh plEnd s0l
  , pr ~ 'PParser prCh prEnd s0r
  , SingParser pl
  , SingParser pr
  ) => SingParser (Then' plCh plEnd s0l prCh prEnd s0r) where
    type PS (Then' plCh plEnd s0l prCh prEnd s0r) =
        SEither
            (PS ('PParser plCh plEnd s0l))
            (STuple2
                (PR ('PParser plCh plEnd s0l))
                (PS ('PParser prCh prEnd s0r)))
    type PR (Then' plCh plEnd s0l prCh prEnd s0r) =
        STuple2
            (PR ('PParser plCh plEnd s0l))
            (PR ('PParser prCh prEnd s0r))
    singParser' = sThen (singParser @pl) (singParser @pr)

-- | Sequence two parsers, running left then right, and return both results.
infixl 4 :<*>:
type (:<*>:)
    :: PParser sl rl
    -> PParser sr rr
    -> PParser (Either sl (rl, sr)) (rl, rr)
type family pl :<*>: pr where
    'PParser plCh plEnd s0l :<*>: 'PParser prCh prEnd s0r =
        Then' plCh plEnd s0l prCh prEnd s0r

type Then'
    :: ParserChSym  sl rl
    -> ParserEndSym sl rl
    -> sl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> PParser (Either sl (rl, sr)) (rl, rr)
type Then' plCh plEnd s0l prCh prEnd s0r =
    'PParser (ThenChSym plCh prCh s0r) (ThenEndSym plEnd prEnd s0r) (Left s0l)

type ThenCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> PParserCh (Either sl (rl, sr)) (rl, rr)
type family ThenCh plCh prCh s0r ch s where
    ThenCh plCh prCh s0r ch (Left  sl) =
        ThenChL prCh s0r ch (plCh @@ ch @@ sl)
    ThenCh plCh prCh s0r ch (Right '(rl, sr)) =
        ThenChR rl (prCh @@ ch @@ sr)

type family ThenChL prCh s0r ch resl where
    ThenChL prCh s0r ch (Cont sl) = Cont (Left  sl)
    ThenChL prCh s0r ch (Done rl) =
        -- 'Done' doesn't consume, so re-parse with the R parser.
        ThenChR rl (prCh @@ ch @@ s0r)
    ThenChL prCh s0r ch (Err  el) = Err  (EThenChL el)

type EThenChL el = EIn "Then(L)" el
eThenChL :: SE el -> SE (EThenChL el)
eThenChL el = withSingE el $ singE

type family ThenChR rl resr where
    ThenChR rl (Cont sr) = Cont (Right '(rl, sr))
    ThenChR rl (Done rr) = Done '(rl, rr)
    ThenChR rl (Err  er) = Err  (EThenChR er)

type EThenChR er = EIn "Then(R)" er
eThenChR :: SE er -> SE (EThenChR er)
eThenChR er = withSingE er $ singE

sThenChR
    :: srl rl
    -> SResult ssr srr resr
    -> SResult (SEither ssl (STuple2 srl ssr)) (STuple2 srl srr)
        (ThenChR rl resr)
sThenChR rl = \case
  SCont sr -> SCont $ SRight $ STuple2 rl sr
  SDone rr -> SDone $ STuple2 rl rr
  SErr  er -> SErr  $ eThenChR er

sThenChSym
    :: SParserChSym ssl srl plCh
    -> SParserChSym ssr srr prCh
    -> ssr sr
    -> SParserChSym (SEither ssl (STuple2 srl ssr)) (STuple2 srl srr)
        (ThenChSym plCh prCh sr)
sThenChSym plCh prCh s0r = Lam2 $ \ch -> \case
  SLeft sl ->
    case plCh @@ ch @@ sl of
      SCont sl' -> SCont $ SLeft sl'
      SDone rl  -> sThenChR rl (prCh @@ ch @@ s0r)
      SErr  el  -> SErr  $ eThenChL el
  SRight (STuple2 rl sr) -> sThenChR rl (prCh @@ ch @@ sr)

type ThenChSym
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym (Either sl (rl, sr)) (rl, rr)
data ThenChSym plCh prCh s0r f
type instance App (ThenChSym plCh prCh s0r) f = ThenChSym1 plCh prCh s0r f

type ThenChSym1
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym1 (Either sl (rl, sr)) (rl, rr)
data ThenChSym1 plCh prCh s0r ch s
type instance App (ThenChSym1 plCh prCh s0r ch) s = ThenCh plCh prCh s0r ch s

type family ThenEnd plEnd prEnd s0r s where
    -- | EOT during R: call R end
    ThenEnd plEnd prEnd s0r (Right '(rl, sr)) = ThenEndR rl    (prEnd @@ sr)
    -- | EOT during L: call L end, pass R end
    ThenEnd plEnd prEnd s0r (Left sl)         = ThenEndL prEnd s0r (plEnd @@ sl)

type family ThenEndR rl res where
    -- | EOT during R, R end succeeds: success
    ThenEndR rl (Right rr) = Right '(rl, rr)
    -- | EOT during R, R end fails: error
    ThenEndR rl (Left  er) = Left  (EThenEndR er)

type EThenEndR er = EIn "Then(R) end" er
eThenEndR :: SE er -> SE (EThenEndR er)
eThenEndR er = withSingE er $ singE

sThenEndR
    :: srl rl
    -> SResultEnd srr res
    -> SResultEnd (STuple2 srl srr) (ThenEndR rl res)
sThenEndR rl = \case
  SRight rr -> SRight $ STuple2 rl rr
  SLeft  er -> SLeft  $ eThenEndR er

type family ThenEndL prEnd s0r res where
    -- | EOT during L, L end succeeds: call R end on initial R state
    ThenEndL prEnd s0r (Right rl) = ThenEndR rl (prEnd @@ s0r)
    -- | EOT during L, L end fails: error
    ThenEndL prEnd s0r (Left  el) = Left (EThenEndL el)

type EThenEndL er = EIn "Then(L) end" er
eThenEndL :: SE er -> SE (EThenEndL er)
eThenEndL er = withSingE er $ singE

sThenEndSym
    :: SParserEndSym ssl srl plEnd
    -> SParserEndSym ssr srr prEnd
    -> ssr s0r
    -> SParserEndSym (SEither ssl (STuple2 srl ssr)) (STuple2 srl srr)
        (ThenEndSym plEnd prEnd s0r)
sThenEndSym plEnd prEnd s0r = Lam $ \case
  SRight (STuple2 rl sr) -> sThenEndR rl (prEnd @@ sr)
  SLeft  sl ->
    case plEnd @@ sl of
      SRight rl -> sThenEndR rl (prEnd @@ s0r)
      SLeft  el -> SLeft $ eThenEndL el

type ThenEndSym
    :: ParserEndSym sl rl
    -> ParserEndSym sr rr
    -> sr
    -> ParserEndSym (Either sl (rl, sr)) (rl, rr)
data ThenEndSym plEnd prEnd s0r s
type instance App (ThenEndSym plEnd prEnd s0r) s = ThenEnd plEnd prEnd s0r s
