{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Then where -- ( (:<*>:), Then' ) where

import Symparsec.Parser.Common
import Singleraeh.Either ( SEither(..) )
import Singleraeh.Tuple ( STuple2(..) )
import DeFun.Core

type SPThen ssl srl ssr srr plCh plEnd sl prCh prEnd sr =
    SParser
        (SEither ssl (STuple2 srl ssr))
        (STuple2 srl srr)
        (Then' plCh plEnd sl prCh prEnd sr)

sThen
    :: SParser ssl srl ('Parser plCh plEnd slInit)
    -> SParser ssr srr ('Parser prCh prEnd srInit)
    -> SPThen  ssl srl ssr srr plCh plEnd slInit prCh prEnd srInit
sThen (SParser plCh plEnd slInit) (SParser prCh prEnd srInit) =
    SParser (thenChSym plCh prCh srInit) (thenEndSym plEnd prEnd) (SLeft slInit)

instance
  -- Shame I can't use pl, pr in associated type synonyms! :(
  ( pl ~ 'Parser plCh plEnd slInit
  , pr ~ 'Parser prCh prEnd srInit
  , SingParser pl
  , SingParser pr
  ) => SingParser (Then' plCh plEnd slInit prCh prEnd srInit) where
    type PS (Then' plCh plEnd slInit prCh prEnd srInit) =
        SEither
            (PS ('Parser plCh plEnd slInit))
            (STuple2
                (PR ('Parser plCh plEnd slInit))
                (PS ('Parser prCh prEnd srInit)))
    type PR (Then' plCh plEnd slInit prCh prEnd srInit) =
        STuple2
            (PR ('Parser plCh plEnd slInit))
            (PR ('Parser prCh prEnd srInit))
    singParser' = sThen (singParser @pl) (singParser @pr)

-- | Sequence two parsers, running left then right, and return both results.
infixl 4 :<*>:
type (:<*>:)
    :: Parser sl rl
    -> Parser sr rr
    -> Parser (Either sl (rl, sr)) (rl, rr)
type family pl :<*>: pr where
    'Parser plCh plEnd sl :<*>: 'Parser prCh prEnd sr =
        Then' plCh plEnd sl prCh prEnd sr

-- TODO thrown plEnd into ThenEndSym because otherwise we get reifying issues.
-- lmao
type Then'
    :: ParserChSym  sl rl
    -> ParserEndSym sl rl
    -> sl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> Parser (Either sl (rl, sr)) (rl, rr)
type Then' plCh plEnd sl prCh prEnd sr =
    'Parser (ThenChSym plCh prCh sr) (ThenEndSym plEnd prEnd) (Left sl)

type ThenCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> PParserCh (Either sl (rl, sr)) (rl, rr)
type family ThenCh plCh prCh sr ch s where
    ThenCh plCh prCh sr ch (Left  sl) =
        ThenL sr (plCh @@ ch @@ sl)
    ThenCh plCh prCh _  ch (Right '(rl, sr)) =
        ThenR rl (prCh @@ ch @@ sr)

type family ThenL sr resl where
    ThenL sr (Cont sl) = Cont (Left  sl)
    ThenL sr (Done rl) = Cont (Right '(rl, sr))
    ThenL sr (Err  el) = Err  (EThenChL el)

type family ThenR rl resr where
    ThenR rl (Cont sr) = Cont (Right '(rl, sr))
    ThenR rl (Done rr) = Done '(rl, rr)
    ThenR rl (Err  er) = Err  (EThenChR er)

thenChSym
    :: SParserChSym ssl srl plCh
    -> SParserChSym ssr srr prCh
    -> ssr sr
    -> SParserChSym (SEither ssl (STuple2 srl ssr)) (STuple2 srl srr)
        (ThenChSym plCh prCh sr)
thenChSym plCh prCh srInit = Lam2 $ \ch -> \case
  SLeft sl ->
    case plCh @@ ch @@ sl of
      SCont sl' -> SCont $ SLeft sl'
      SDone rl  -> SCont $ SRight (STuple2 rl srInit)
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

thenEndSym
    :: SParserEndSym ssl srl plEnd
    -> SParserEndSym ssr srr prEnd
    -> SParserEndSym (SEither ssl (STuple2 srl ssr)) (STuple2 srl srr)
        (ThenEndSym plEnd prEnd)
thenEndSym _plEnd prEnd = Lam $ \case
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
