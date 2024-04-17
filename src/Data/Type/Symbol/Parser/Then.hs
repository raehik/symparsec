{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Then where

import Data.Type.Symbol.Parser.Internal
import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@), type App )

type PThen
    :: ParserSym' sl rl
    -> ParserSym' sr rr
    -> ParserSym' (Either sl (rl, sr)) (rl, rr)
type family PThen pl pr where
    PThen '(plCh, plEnd, sl) '(prCh, prEnd, sr) =
        '(ThenSym plCh prCh sr, ThenEndSym prEnd, 'Left sl)

type Then
    :: ParserSym sl rl
    -> ParserSym sr rr
    -> sr
    -> Parser (Either sl (rl, sr)) (rl, rr)
type family Then plCh prCh sr ch s where
    Then plCh prCh sr ch ('Left  sl) =
        ThenL sr (plCh @@ ch @@ sl)
    Then plCh prCh _  ch ('Right '(rl, sr)) =
        ThenR rl (prCh @@ ch @@ sr)

type family ThenL sr resl where
    ThenL sr ('Err  el) = 'Err  ('Text "then: left error" :$$: el)
    ThenL sr ('Cont sl) = 'Cont ('Left  sl)
    ThenL sr ('Done rl) = 'Cont ('Right '(rl, sr))

type family ThenR rl resr where
    ThenR rl ('Err  er) = 'Err  ('Text "then: right error" :$$: er)
    ThenR rl ('Cont sr) = 'Cont ('Right '(rl, sr))
    ThenR rl ('Done rr) = 'Done '(rl, rr)

type family ThenEnd prEnd s where
    ThenEnd prEnd ('Left sl) = 'Left ('Text "then: ended during left")
    ThenEnd prEnd ('Right '(rl, sr)) =
        ThenEnd' rl (prEnd @@ sr)

type family ThenEnd' rl s where
    ThenEnd' rl ('Left  er) = 'Left  ('Text "then: right end error" :$$: er)
    ThenEnd' rl ('Right rr) = 'Right '(rl, rr)

type ThenSym
    :: ParserSym sl rl
    -> ParserSym sr rr
    -> sr
    -> ParserSym (Either sl (rl, sr)) (rl, rr)
data ThenSym plCh prCh sr f
type instance App (ThenSym plCh prCh sr) f = ThenSym1 plCh prCh sr f

type ThenSym1
    :: ParserSym sl rl
    -> ParserSym sr rr
    -> sr
    -> Char -> Either sl (rl, sr) ~> Result (Either sl (rl, sr)) (rl, rr)
data ThenSym1 plCh prCh sr ch s
type instance App (ThenSym1 plCh prCh sr ch) s = Then plCh prCh sr ch s

type ThenEndSym
    :: ParserEndSym sr rr
    -> ParserEndSym (Either sl (rl, sr)) (rl, rr)
data ThenEndSym prEnd s
type instance App (ThenEndSym prEnd) s = ThenEnd prEnd s
