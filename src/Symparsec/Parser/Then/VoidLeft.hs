{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Then.VoidLeft ( (:*>:) ) where

import Symparsec.Parser.Common

-- | Sequence two parsers, running left then right, and discard the return value
--   of the left parser.
infixl 4 :*>:
type (:*>:)
    :: Parser sl rl
    -> Parser sr rr
    -> Parser (Either sl sr) rr
type family pl :*>: pr where
    'Parser plCh plEnd sl :*>: 'Parser prCh prEnd sr =
        'Parser (ThenVLChSym plCh prCh sr) (ThenVLEndSym prEnd) (Left sl)

type ThenVLCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> PParserCh (Either sl sr) rr
type family ThenVLCh plCh prCh sr ch s where
    ThenVLCh plCh prCh sr ch (Left  sl) =
        ThenVLL sr (plCh @@ ch @@ sl)
    ThenVLCh plCh prCh _  ch (Right sr) =
        ThenVLR (prCh @@ ch @@ sr)

type family ThenVLL sr resl where
    ThenVLL sr (Err  el) = Err  (EIn "ThenVL(L)" el)
    ThenVLL sr (Cont sl) = Cont (Left  sl)
    ThenVLL sr (Done rl) = Cont (Right sr)

type family ThenVLR resr where
    ThenVLR (Err  er) = Err  (EIn "ThenVL(R)" er)
    ThenVLR (Cont sr) = Cont (Right sr)
    ThenVLR (Done rr) = Done rr

type family ThenVLEnd prEnd s where
    ThenVLEnd prEnd (Left  sl) =
        Left (EBase "ThenVL" (Text "ended during left"))
    ThenVLEnd prEnd (Right sr) = ThenVLEnd' (prEnd @@ sr)

type family ThenVLEnd' s where
    ThenVLEnd' (Left  er) = Left  (EIn "ThenVL(R)" er)
    ThenVLEnd' (Right rr) = Right rr

type ThenVLChSym
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym (Either sl sr) rr
data ThenVLChSym plCh prCh sr f
type instance App (ThenVLChSym plCh prCh sr) f = ThenVLChSym1 plCh prCh sr f

type ThenVLChSym1
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> Char -> Either sl sr ~> PResult (Either sl sr) rr
data ThenVLChSym1 plCh prCh sr ch s
type instance App (ThenVLChSym1 plCh prCh sr ch) s = ThenVLCh plCh prCh sr ch s

type ThenVLEndSym
    :: ParserEndSym sr rr
    -> ParserEndSym (Either sl sr) rr
data ThenVLEndSym prEnd s
type instance App (ThenVLEndSym prEnd) s = ThenVLEnd prEnd s
