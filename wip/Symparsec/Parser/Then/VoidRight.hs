{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Then.VoidRight where

import Symparsec.Parser.Common
import Singleraeh.Either ( SEither(..) )
import Singleraeh.Tuple ( STuple2(..) )
import DeFun.Core

type SPThenVR ssl srl ssr srr plCh plEnd s0l prCh prEnd s0r =
    SParser
        (SEither ssl (STuple2 srl ssr))
        srl
        (ThenVR' plCh plEnd s0l prCh prEnd s0r)

sThenVR
    :: SParser  ssl srl ('PParser plCh plEnd s0l)
    -> SParser  ssr srr ('PParser prCh prEnd s0r)
    -> SPThenVR ssl srl ssr srr plCh plEnd s0l prCh prEnd s0r
sThenVR (SParser plCh plEnd s0l) (SParser prCh prEnd s0r) =
    SParser (sThenVRChSym plCh prCh s0r) (sThenVREndSym plEnd prEnd s0r) (SLeft s0l)

instance
  -- Shame I can't use pl, pr in associated type synonyms! :(
  ( pl ~ 'PParser plCh plEnd s0l
  , pr ~ 'PParser prCh prEnd s0r
  , SingParser pl
  , SingParser pr
  ) => SingParser (ThenVR' plCh plEnd s0l prCh prEnd s0r) where
    type PS (ThenVR' plCh plEnd s0l prCh prEnd s0r) =
        SEither
            (PS ('PParser plCh plEnd s0l))
            (STuple2
                (PR ('PParser plCh plEnd s0l))
                (PS ('PParser prCh prEnd s0r)))
    type PR (ThenVR' plCh plEnd s0l prCh prEnd s0r) =
        PR ('PParser plCh plEnd s0l)
    singParser' = sThenVR (singParser @pl) (singParser @pr)

-- | Sequence two parsers, running left then right, and discard the return value
--   of the right parser.
infixl 4 :<*:
type (:<*:)
    :: PParser sl rl
    -> PParser sr rr
    -> PParser (Either sl (rl, sr)) rl
type family pl :<*: pr where
    'PParser plCh plEnd s0l :<*: 'PParser prCh prEnd s0r =
        ThenVR' plCh plEnd s0l prCh prEnd s0r

type ThenVR'
    :: ParserChSym  sl rl
    -> ParserEndSym sl rl
    -> sl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> PParser (Either sl (rl, sr)) rl
type ThenVR' plCh plEnd s0l prCh prEnd s0r =
    'PParser (ThenVRChSym plCh prCh s0r) (ThenVREndSym plEnd prEnd s0r) (Left s0l)

type ThenVRCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> PParserCh (Either sl (rl, sr)) rl
type family ThenVRCh plCh prCh s0r ch s where
    ThenVRCh plCh prCh s0r ch (Left  sl) =
        ThenVRChL prCh s0r ch (plCh @@ ch @@ sl)
    ThenVRCh plCh prCh s0r ch (Right '(rl, sr)) =
        ThenVRChR rl (prCh @@ ch @@ sr)

type family ThenVRChL prCh s0r ch resl where
    ThenVRChL prCh s0r ch (Cont sl) = Cont (Left  sl)
    ThenVRChL prCh s0r ch (Done rl) =
        -- 'Done' doesn't consume, so re-parse with the R parser.
        ThenVRChR rl (prCh @@ ch @@ s0r)
    ThenVRChL prCh s0r ch (Err  el) = Err  (EThenVRChL el)

type EThenVRChL el = EIn "ThenVR(L)" el
eThenVRChL :: SE el -> SE (EThenVRChL el)
eThenVRChL el = withSingE el $ singE

type family ThenVRChR rl resr where
    ThenVRChR rl (Cont sr) = Cont (Right '(rl, sr))
    ThenVRChR rl (Done rr) = Done rl
    ThenVRChR rl (Err  er) = Err  (EThenVRChR er)

type EThenVRChR er = EIn "ThenVR(R)" er
eThenVRChR :: SE er -> SE (EThenVRChR er)
eThenVRChR er = withSingE er $ singE

sThenVRChR
    :: srl rl
    -> SResult ssr srr resr
    -> SResult (SEither ssl (STuple2 srl ssr)) srl (ThenVRChR rl resr)
sThenVRChR rl = \case
  SCont  sr -> SCont $ SRight $ STuple2 rl sr
  SDone _rr -> SDone rl
  SErr   er -> SErr  $ eThenVRChR er

sThenVRChSym
    :: SParserChSym ssl srl plCh
    -> SParserChSym ssr srr prCh
    -> ssr sr
    -> SParserChSym (SEither ssl (STuple2 srl ssr)) srl
        (ThenVRChSym plCh prCh sr)
sThenVRChSym plCh prCh s0r = Lam2 $ \ch -> \case
  SLeft sl ->
    case plCh @@ ch @@ sl of
      SCont sl' -> SCont $ SLeft sl'
      SDone rl  -> sThenVRChR rl (prCh @@ ch @@ s0r)
      SErr  el  -> SErr  $ eThenVRChL el
  SRight (STuple2 rl sr) -> sThenVRChR rl (prCh @@ ch @@ sr)

type ThenVRChSym
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym (Either sl (rl, sr)) rl
data ThenVRChSym plCh prCh s0r f
type instance App (ThenVRChSym plCh prCh s0r) f = ThenVRChSym1 plCh prCh s0r f

type ThenVRChSym1
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym1 (Either sl (rl, sr)) rl
data ThenVRChSym1 plCh prCh s0r ch s
type instance App (ThenVRChSym1 plCh prCh s0r ch) s = ThenVRCh plCh prCh s0r ch s

type family ThenVREnd plEnd prEnd s0r s where
    -- | EOT during R: call R end
    ThenVREnd plEnd prEnd s0r (Right '(rl, sr)) = ThenVREndR rl    (prEnd @@ sr)
    -- | EOT during L: call L end, pass R end
    ThenVREnd plEnd prEnd s0r (Left sl)         = ThenVREndL prEnd s0r (plEnd @@ sl)

type family ThenVREndR rl res where
    -- | EOT during R, R end succeeds: success
    ThenVREndR rl (Right rr) = Right rl
    -- | EOT during R, R end fails: error
    ThenVREndR rl (Left  er) = Left  (EThenVREndR er)

type EThenVREndR er = EIn "ThenVR(R) end" er
eThenVREndR :: SE er -> SE (EThenVREndR er)
eThenVREndR er = withSingE er $ singE

sThenVREndR
    :: srl rl
    -> SResultEnd srr res
    -> SResultEnd srl (ThenVREndR rl res)
sThenVREndR rl = \case
  SRight _rr -> SRight rl
  SLeft   er -> SLeft  $ eThenVREndR er

type family ThenVREndL prEnd s0r res where
    -- | EOT during L, L end succeeds: call R end on initial R state
    ThenVREndL prEnd s0r (Right rl) = ThenVREndR rl (prEnd @@ s0r)
    -- | EOT during L, L end fails: error
    ThenVREndL prEnd s0r (Left  el) = Left (EThenVREndL el)

type EThenVREndL er = EIn "ThenVR(L) end" er
eThenVREndL :: SE er -> SE (EThenVREndL er)
eThenVREndL er = withSingE er $ singE

sThenVREndSym
    :: SParserEndSym ssl srl plEnd
    -> SParserEndSym ssr srr prEnd
    -> ssr s0r
    -> SParserEndSym (SEither ssl (STuple2 srl ssr)) srl
        (ThenVREndSym plEnd prEnd s0r)
sThenVREndSym plEnd prEnd s0r = Lam $ \case
  SRight (STuple2 rl sr) -> sThenVREndR rl (prEnd @@ sr)
  SLeft  sl ->
    case plEnd @@ sl of
      SRight rl -> sThenVREndR rl (prEnd @@ s0r)
      SLeft  el -> SLeft $ eThenVREndL el

type ThenVREndSym
    :: ParserEndSym sl rl
    -> ParserEndSym sr rr
    -> sr
    -> ParserEndSym (Either sl (rl, sr)) rl
data ThenVREndSym plEnd prEnd s0r s
type instance App (ThenVREndSym plEnd prEnd s0r) s = ThenVREnd plEnd prEnd s0r s
