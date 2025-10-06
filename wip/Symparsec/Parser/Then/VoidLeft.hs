{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Then.VoidLeft where

import Symparsec.Parser.Common
import Singleraeh.Either ( SEither(..) )
import DeFun.Core

type SPThenVL ssl srl ssr srr plCh plEnd s0l prCh prEnd s0r =
    SParser
        (SEither ssl ssr)
        srr
        (ThenVL' plCh plEnd s0l prCh prEnd s0r)

sThenVL
    :: SParser   ssl srl ('PParser plCh plEnd s0l)
    -> SParser   ssr srr ('PParser prCh prEnd s0r)
    -> SPThenVL  ssl srl ssr srr plCh plEnd s0l prCh prEnd s0r
sThenVL (SParser plCh plEnd s0l) (SParser prCh prEnd s0r) =
    SParser (sThenVLChSym plCh prCh s0r) (sThenVLEndSym plEnd prEnd s0r) (SLeft s0l)

instance
  ( pl ~ 'PParser plCh plEnd s0l
  , pr ~ 'PParser prCh prEnd s0r
  , SingParser pl
  , SingParser pr
  ) => SingParser (ThenVL' plCh plEnd s0l prCh prEnd s0r) where
    type PS (ThenVL' plCh plEnd s0l prCh prEnd s0r) =
        SEither
            (PS ('PParser plCh plEnd s0l))
            (PS ('PParser prCh prEnd s0r))
    type PR (ThenVL' plCh plEnd s0l prCh prEnd s0r) =
        PR ('PParser prCh prEnd s0r)
    singParser' = sThenVL (singParser @pl) (singParser @pr)

-- | Sequence two parsers, running left then right, and discard the return value
--   of the left parser.
infixl 4 :*>:
type (:*>:)
    :: PParser sl rl
    -> PParser sr rr
    -> PParser (Either sl sr) rr
type family pl :*>: pr where
    'PParser plCh plEnd s0l :*>: 'PParser prCh prEnd s0r =
        ThenVL' plCh plEnd s0l prCh prEnd s0r

type ThenVL'
    :: ParserChSym  sl rl
    -> ParserEndSym sl rl
    -> sl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> PParser (Either sl sr) rr
type ThenVL' plCh plEnd s0l prCh prEnd s0r =
    'PParser (ThenVLChSym plCh prCh s0r) (ThenVLEndSym plEnd prEnd s0r) (Left s0l)

type ThenVLCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> PParserCh (Either sl sr) rr
type family ThenVLCh plCh prCh s0r ch s where
    ThenVLCh plCh prCh s0r ch (Left  sl) =
        ThenVLChL prCh s0r ch (plCh @@ ch @@ sl)
    ThenVLCh plCh prCh s0r ch (Right sr) =
        ThenVLChR (prCh @@ ch @@ sr)

type family ThenVLChL prCh s0r ch resl where
    ThenVLChL prCh s0r ch (Cont sl) = Cont (Left  sl)
    ThenVLChL prCh s0r ch (Done rl) =
        -- 'Done' doesn't consume, so re-parse with the R parser.
        ThenVLChR (prCh @@ ch @@ s0r)
    ThenVLChL prCh s0r ch (Err  el) = Err  (EThenVLChL el)

type EThenVLChL el = EIn "ThenVL(L)" el
eThenVLChL :: SE el -> SE (EThenVLChL el)
eThenVLChL el = withSingE el $ singE

type family ThenVLChR resr where
    ThenVLChR (Cont sr) = Cont (Right sr)
    ThenVLChR (Done rr) = Done rr
    ThenVLChR (Err  er) = Err  (EThenVLChR er)

type EThenVLChR er = EIn "ThenVL(R)" er
eThenVLChR :: SE er -> SE (EThenVLChR er)
eThenVLChR er = withSingE er $ singE

sThenVLChR
    :: SResult ssr srr resr
    -> SResult (SEither ssl ssr) srr (ThenVLChR resr)
sThenVLChR = \case
  SCont sr -> SCont $ SRight sr
  SDone rr -> SDone rr
  SErr  er -> SErr  $ eThenVLChR er

sThenVLChSym
    :: SParserChSym ssl srl plCh
    -> SParserChSym ssr srr prCh
    -> ssr sr
    -> SParserChSym (SEither ssl ssr) srr
        (ThenVLChSym plCh prCh sr)
sThenVLChSym plCh prCh s0r = Lam2 $ \ch -> \case
  SLeft  sl ->
    case plCh @@ ch @@ sl of
      SCont  sl' -> SCont $ SLeft sl'
      SDone _rl  -> sThenVLChR (prCh @@ ch @@ s0r)
      SErr   el  -> SErr  $ eThenVLChL el
  SRight sr -> sThenVLChR (prCh @@ ch @@ sr)

type ThenVLChSym
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym (Either sl sr) rr
data ThenVLChSym plCh prCh sr f
type instance App (ThenVLChSym plCh prCh s0r) f = ThenVLChSym1 plCh prCh s0r f

type ThenVLChSym1
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym1 (Either sl sr) rr
data ThenVLChSym1 plCh prCh s0r ch s
type instance App (ThenVLChSym1 plCh prCh s0r ch) s = ThenVLCh plCh prCh s0r ch s

type family ThenVLEnd plEnd prEnd s0r s where
    -- | EOT during R: call R end
    ThenVLEnd plEnd prEnd s0r (Right sr) = ThenVLEndR           (prEnd @@ sr)
    -- | EOT during L: call L end, pass R end
    ThenVLEnd plEnd prEnd s0r (Left  sl) = ThenVLEndL prEnd s0r (plEnd @@ sl)

type family ThenVLEndR res where
    -- | EOT during R, R end succeeds: success
    ThenVLEndR (Right rr) = Right rr
    -- | EOT during R, R end fails: error
    ThenVLEndR (Left  er) = Left  (EThenVLEndR er)

type EThenVLEndR er = EIn "ThenVL(R) end" er
eThenVLEndR :: SE er -> SE (EThenVLEndR er)
eThenVLEndR er = withSingE er $ singE

sThenVLEndR
    :: SResultEnd srr res
    -> SResultEnd srr (ThenVLEndR res)
sThenVLEndR = \case
  SRight rr -> SRight rr
  SLeft  er -> SLeft  $ eThenVLEndR er

type family ThenVLEndL prEnd s0r res where
    -- | EOT during L, L end succeeds: call R end on initial R state
    ThenVLEndL prEnd s0r (Right rl) = ThenVLEndR (prEnd @@ s0r)
    -- | EOT during L, L end fails: error
    ThenVLEndL prEnd s0r (Left  el) = Left (EThenVLEndL el)

type EThenVLEndL er = EIn "ThenVL(L) end" er
eThenVLEndL :: SE er -> SE (EThenVLEndL er)
eThenVLEndL er = withSingE er $ singE

sThenVLEndSym
    :: SParserEndSym ssl srl plEnd
    -> SParserEndSym ssr srr prEnd
    -> ssr s0r
    -> SParserEndSym (SEither ssl ssr) srr
        (ThenVLEndSym plEnd prEnd s0r)
sThenVLEndSym plEnd prEnd s0r = Lam $ \case
  SRight sr -> sThenVLEndR (prEnd @@ sr)
  SLeft  sl ->
    case plEnd @@ sl of
      SRight _rl -> sThenVLEndR (prEnd @@ s0r)
      SLeft   el -> SLeft $ eThenVLEndL el

type ThenVLEndSym
    :: ParserEndSym sl rl
    -> ParserEndSym sr rr
    -> sr
    -> ParserEndSym (Either sl sr) rr
data ThenVLEndSym plEnd prEnd s0r s
type instance App (ThenVLEndSym plEnd prEnd s0r) s = ThenVLEnd plEnd prEnd s0r s
