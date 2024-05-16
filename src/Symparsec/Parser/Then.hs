{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Then ( (:<*>:), Then' ) where

import Symparsec.Parser.Common

-- | Sequence two parsers, running left then right, and return both results.
infixl 4 :<*>:
type (:<*>:)
    :: ParserSym sl rl
    -> ParserSym sr rr
    -> ParserSym (Either sl (rl, sr)) (rl, rr)
type family pl :<*>: pr where
    'ParserSym plCh plEnd sl :<*>: 'ParserSym prCh prEnd sr =
        Then' plCh plEnd sl prCh prEnd sr
--        'ParserSym (ThenChSym plCh prCh sr) (ThenEndSym prEnd) (Left sl)

-- TODO thrown plEnd into ThenEndSym because otherwise we get reifying issues.
-- lmao
type Then'
    :: ParserChSym  sl rl
    -> ParserEndSym sl rl
    -> sl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> ParserSym (Either sl (rl, sr)) (rl, rr)
type Then' plCh plEnd sl prCh prEnd sr =
    'ParserSym (ThenChSym plCh prCh sr) (ThenEndSym plEnd prEnd) (Left sl)

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
    ThenL sr (Err  el) = Err  (EIn "Then(L)" el)

type family ThenR rl resr where
    ThenR rl (Cont sr) = Cont (Right '(rl, sr))
    ThenR rl (Done rr) = Done '(rl, rr)
    ThenR rl (Err  er) = Err  (EIn "Then(R)" er)

type family ThenEnd prEnd s where
    ThenEnd prEnd (Right '(rl, sr)) =
        ThenEnd' rl (prEnd @@ sr)
    ThenEnd prEnd (Left sl) = Left (EBase "Then" (Text "ended during left"))

type family ThenEnd' rl s where
    ThenEnd' rl (Right rr) = Right '(rl, rr)
    ThenEnd' rl (Left  er) = Left  (EIn "Then(R)" er)

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

type ThenEndSym
    :: ParserEndSym sl rl
    -> ParserEndSym sr rr
    -> ParserEndSym (Either sl (rl, sr)) (rl, rr)
data ThenEndSym plEnd prEnd s
type instance App (ThenEndSym plEnd prEnd) s = ThenEnd prEnd s
