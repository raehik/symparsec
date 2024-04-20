{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Parser.Then.VoidRight ( ThenVR ) where

import Data.Type.Symbol.Parser.Types
import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@), type App )

type ThenVR
    :: Parser sl rl
    -> Parser sr rr
    -> Parser (Either sl (rl, sr)) rl
type family ThenVR pl pr where
    ThenVR '(plCh, plEnd, sl) '(prCh, prEnd, sr) =
        '(ThenVRChSym plCh prCh sr, ThenVREndSym prEnd, Left sl)

type ThenVRCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserCh (Either sl (rl, sr)) rl
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
    -> Char -> Either sl (rl, sr) ~> Result (Either sl (rl, sr)) rl
data ThenVRChSym1 plCh prCh sr ch s
type instance App (ThenVRChSym1 plCh prCh sr ch) s = ThenVRCh plCh prCh sr ch s

type ThenVREndSym
    :: ParserEndSym sr rr
    -> ParserEndSym (Either sl (rl, sr)) rl
data ThenVREndSym prEnd s
type instance App (ThenVREndSym prEnd) s = ThenVREnd prEnd s
