{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Parser.Then ( Then ) where

import Data.Type.Symbol.Parser.Types
import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@), type App )

type Then
    :: Parser sl rl
    -> Parser sr rr
    -> Parser (Either sl (rl, sr)) (rl, rr)
type family Then pl pr where
    Then '(plCh, plEnd, sl) '(prCh, prEnd, sr) =
        '(ThenChSym plCh prCh sr, ThenEndSym prEnd, Left sl)

type ThenCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserCh (Either sl (rl, sr)) (rl, rr)
type family ThenCh plCh prCh sr ch s where
    ThenCh plCh prCh sr ch (Left  sl) =
        ThenL sr (plCh @@ ch @@ sl)
    ThenCh plCh prCh _  ch (Right '(rl, sr)) =
        ThenR rl (prCh @@ ch @@ sr)

type family ThenL sr resl where
    ThenL sr (Err  el) = Err  (EIn "Then(L)" el)
    ThenL sr (Cont sl) = Cont (Left  sl)
    ThenL sr (Done rl) = Cont (Right '(rl, sr))

type family ThenR rl resr where
    ThenR rl (Err  er) = Err  (EIn "Then(R)" er)
    ThenR rl (Cont sr) = Cont (Right '(rl, sr))
    ThenR rl (Done rr) = Done '(rl, rr)

type family ThenEnd prEnd s where
    ThenEnd prEnd (Left sl) = Left (EBase "Then" (Text "ended during left"))
    ThenEnd prEnd (Right '(rl, sr)) =
        ThenEnd' rl (prEnd @@ sr)

type family ThenEnd' rl s where
    ThenEnd' rl (Left  er) = Left  (EIn "Then(R)" er)
    ThenEnd' rl (Right rr) = Right '(rl, rr)

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
    -> Char -> Either sl (rl, sr) ~> Result (Either sl (rl, sr)) (rl, rr)
data ThenChSym1 plCh prCh sr ch s
type instance App (ThenChSym1 plCh prCh sr ch) s = ThenCh plCh prCh sr ch s

type ThenEndSym
    :: ParserEndSym sr rr
    -> ParserEndSym (Either sl (rl, sr)) (rl, rr)
data ThenEndSym prEnd s
type instance App (ThenEndSym prEnd) s = ThenEnd prEnd s
