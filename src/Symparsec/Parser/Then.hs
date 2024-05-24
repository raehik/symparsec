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
    SParser (sThenChSym plCh prCh s0r) (sThenEndSym plEnd prEnd) (SLeft s0l)

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

-- TODO thrown plEnd into ThenEndSym because otherwise we get reifying issues.
-- lmao
type Then'
    :: ParserChSym  sl rl
    -> ParserEndSym sl rl
    -> sl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> PParser (Either sl (rl, sr)) (rl, rr)
type Then' plCh plEnd s0l prCh prEnd s0r =
    'PParser (ThenChSym plCh prCh s0r) (ThenEndSym plEnd prEnd) (Left s0l)

type ThenCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> PParserCh (Either sl (rl, sr)) (rl, rr)
type family ThenCh plCh prCh s0r ch s where
    ThenCh plCh prCh s0r ch (Left  sl) =
        ThenChL prCh s0r ch (plCh @@ ch @@ sl)
    ThenCh plCh prCh _  ch (Right '(rl, sr)) =
        ThenChR rl (prCh @@ ch @@ sr)

type family ThenChL prCh s0r ch resl where
    ThenChL prCh s0r ch (Cont sl) = Cont (Left  sl)
    ThenChL prCh s0r ch (Done rl) = ThenChR rl (prCh @@ ch @@ s0r)
    ThenChL prCh s0r ch (Err  el) = Err  (EThenChL el)

type family ThenChR rl resr where
    ThenChR rl (Cont sr) = Cont (Right '(rl, sr))
    ThenChR rl (Done rr) = Done '(rl, rr)
    ThenChR rl (Err  er) = Err  (EThenChR er)

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
  SRight (STuple2 rl sr) ->
    case prCh @@ ch @@ sr of
      SCont sr' -> SCont $ SRight $ STuple2 rl sr'
      SDone rr  -> SDone $ STuple2 rl rr
      SErr  er  -> SErr  $ eThenChR er

type ThenChSym
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym (Either sl (rl, sr)) (rl, rr)
data ThenChSym plCh prCh sr f
type instance App (ThenChSym plCh prCh sr) f = ThenChSym1 plCh prCh sr f

type ThenChSym1
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> Char -> Either sl (rl, sr) ~> PResult (Either sl (rl, sr)) (rl, rr)
data ThenChSym1 plCh prCh sr ch s
type instance App (ThenChSym1 plCh prCh sr ch) s = ThenCh plCh prCh sr ch s

type family ThenEnd prEnd s where
    ThenEnd prEnd (Right '(rl, sr)) = ThenEnd' rl (prEnd @@ sr)
    ThenEnd prEnd (Left sl)         = Left EThenEndL

type EThenEndL = EBase "Then" (Text "ended during left")

type family ThenEnd' rl s where
    ThenEnd' rl (Right rr) = Right '(rl, rr)
    ThenEnd' rl (Left  er) = Left  (EThenChR er)

type EThenChL el = EIn "Then(L)" el
eThenChL :: SE el -> SE (EThenChL el)
eThenChL el = withSingE el $ singE

type EThenChR er = EIn "Then(R)" er
eThenChR :: SE er -> SE (EThenChR er)
eThenChR er = withSingE er $ singE

sThenEndSym
    :: SParserEndSym ssl srl plEnd
    -> SParserEndSym ssr srr prEnd
    -> SParserEndSym (SEither ssl (STuple2 srl ssr)) (STuple2 srl srr)
        (ThenEndSym plEnd prEnd)
sThenEndSym _plEnd prEnd = Lam $ \case
  SRight (STuple2 rl sr) ->
    case prEnd @@ sr of
      SRight rr -> SRight $ STuple2 rl rr
      SLeft  er -> SLeft  $ eThenChR er
  SLeft  _sl -> SLeft singE

-- TODO plEnd is unused. need it for reifying to work. lmao
type ThenEndSym
    :: ParserEndSym sl rl
    -> ParserEndSym sr rr
    -> ParserEndSym (Either sl (rl, sr)) (rl, rr)
data ThenEndSym plEnd prEnd s
type instance App (ThenEndSym plEnd prEnd) s = ThenEnd prEnd s
