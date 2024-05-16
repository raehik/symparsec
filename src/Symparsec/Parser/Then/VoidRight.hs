{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Then.VoidRight ( (:<*:) ) where

import Symparsec.Parser.Common

-- | Sequence two parsers, running left then right, and discard the return value
--   of the right parser.
--
-- Consider using 'Data.Type.Symbol.Parser.Parser.Then.VoidLeft.:*>:' instead,
-- which is simpler and potentially faster since we parse left-to-right.
infixl 4 :<*:
type (:<*:)
    :: ParserSym sl rl
    -> ParserSym sr rr
    -> ParserSym (Either sl (rl, sr)) rl
type family pl :<*: pr where
    'ParserSym plCh plEnd sl :<*: 'ParserSym prCh prEnd sr =
        'ParserSym (ThenVRChSym plCh prCh sr) (ThenVREndSym prEnd) (Left sl)

type ThenVRCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> PParserCh (Either sl (rl, sr)) rl
type family ThenVRCh plCh prCh sr ch s where
    ThenVRCh plCh prCh sr ch (Left  sl) =
        ThenVRL sr (plCh @@ ch @@ sl)
    ThenVRCh plCh prCh _  ch (Right '(rl, sr)) =
        ThenVRR rl (prCh @@ ch @@ sr)

type family ThenVRL sr resl where
    ThenVRL sr (Err  el) = Err  (EIn "ThenVR(L)" el)
    ThenVRL sr (Cont sl) = Cont (Left  sl)
    ThenVRL sr (Done rl) = Cont (Right '(rl, sr))

type family ThenVRR rl resr where
    ThenVRR rl (Err  er) = Err  (EIn "ThenVR(R)" er)
    ThenVRR rl (Cont sr) = Cont (Right '(rl, sr))
    ThenVRR rl (Done rr) = Done rl

type family ThenVREnd prEnd s where
    ThenVREnd prEnd (Left  sl) =
        Left (EBase "ThenVR" (Text "ended during left"))
    ThenVREnd prEnd (Right '(rl, sr)) =
        ThenVREnd' rl (prEnd @@ sr)

type family ThenVREnd' rl s where
    ThenVREnd' rl (Left  er) = Left  (EIn "ThenVR(R)" er)
    ThenVREnd' rl (Right rr) = Right rl

type ThenVRChSym
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym (Either sl (rl, sr)) rl
data ThenVRChSym plCh prCh sr f
type instance App (ThenVRChSym plCh prCh sr) f = ThenVRChSym1 plCh prCh sr f

type ThenVRChSym1
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> Char -> Either sl (rl, sr) ~> PResult (Either sl (rl, sr)) rl
data ThenVRChSym1 plCh prCh sr ch s
type instance App (ThenVRChSym1 plCh prCh sr ch) s = ThenVRCh plCh prCh sr ch s

type ThenVREndSym
    :: ParserEndSym sr rr
    -> ParserEndSym (Either sl (rl, sr)) rl
data ThenVREndSym prEnd s
type instance App (ThenVREndSym prEnd) s = ThenVREnd prEnd s
