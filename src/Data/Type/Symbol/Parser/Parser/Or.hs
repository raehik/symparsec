{-# LANGUAGE UndecidableInstances #-}

-- TODO errors won't work properly with this (you'll have to ignore the
-- top-level index/char error, it'll be wrong)

--module Data.Type.Symbol.Parser.Parser.Or ( Or ) where
module Data.Type.Symbol.Parser.Parser.Or where

import Data.Type.Symbol.Parser.Types
import Data.Type.Symbol.Parser.Common
import DeFun.Core ( type (@@), type App )
import Data.Type.List ( Reverse )

type Or
    :: Parser sl rl
    -> Parser sr rr
    -> Parser (Either (sl, [Char]) sr) (Either rl rr)
type family Or pl pr where
    Or '(plCh, plEnd, sl) '(prCh, prEnd, sr) =
        '(OrChSym plCh prCh sr, OrEndSym plEnd prEnd, Left '(sl, '[]))

type OrCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserCh (Either (sl, [Char]) sr) (Either rl rr)
type family OrCh plCh prCh sr ch s where
    OrCh plCh prCh sr ch (Left  '(sl, chs)) =
        OrChL prCh sr (ch : chs) (plCh @@ ch @@ sl)
    OrCh plCh prCh _  ch (Right sr) =
        OrChR (prCh @@ ch @@ sr)

type OrChL
    :: ParserChSym sr rr
    -> sr
    -> [Char]
    -> Result sl rl
    -> Result (Either (sl, [Char]) sr) (Either rl rr)
type family OrChL prCh sr chs resl where
    OrChL _    _  chs (Cont sl) = Cont (Left  '(sl, chs))
    OrChL _    _  chs (Done rl) = Done (Left  rl)
    OrChL prCh sr chs (Err  _ ) =
        OrChLReplay prCh (Reverse chs) (Cont sr)

type family OrChLReplay prCh chs resr where
    OrChLReplay prCh (ch : '[]) (Cont sr) = OrChR (prCh @@ ch @@ sr)
    OrChLReplay prCh (ch : chs) (Cont sr) =
        OrChLReplay prCh chs (prCh @@ ch @@ sr)
    OrChLReplay prCh chs        (Err  er) = Err er -- TODO
    OrChLReplay prCh chs        (Done rr) = Err
        (EBase "Or" (ErrParserLimitation "cannot parse less on right of Or"))

type family OrChR resr where
    OrChR (Cont sr) = Cont (Right sr)
    OrChR (Done rr) = Done (Right rr)
    OrChR (Err  er) = Err er -- TODO

type OrEnd
    :: ParserEndSym sl rl
    -> ParserEndSym sr rr
    -> ParserEnd (Either (sl, [Char]) sr) (Either rl rr)
type family OrEnd plEnd prEnd res where
    OrEnd plEnd prEnd (Left  '(sl, chs)) = OrEndL (plEnd @@ sl)
    OrEnd plEnd prEnd (Right sr)         = OrEndR (prEnd @@ sr)

type family OrEndL s where
    OrEndL (Left  el) = Left el -- TODO
    OrEndL (Right rl) = Right (Left rl)

type family OrEndR s where
    OrEndR (Left  er) = Left er -- TODO
    OrEndR (Right rr) = Right (Right rr)

data OrChSym plCh prCh sr f
type instance App (OrChSym plCh prCh sr) f = OrChSym1 plCh prCh sr f

data OrChSym1 plCh prCh sr ch s
type instance App (OrChSym1 plCh prCh sr ch) s = OrCh plCh prCh sr ch s

data OrEndSym plEnd prEnd s
type instance App (OrEndSym plEnd prEnd) s = OrEnd plEnd prEnd s
