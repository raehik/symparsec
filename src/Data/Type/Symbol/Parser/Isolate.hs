{-# LANGUAGE UndecidableInstances #-}

-- TODO improve errors (I was lazy)

module Data.Type.Symbol.Parser.Isolate where

import Data.Type.Symbol.Parser.Internal
import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@), type App )

type Isolate :: Natural -> Parser s r -> Parser (Natural, s) r
type family Isolate n p where
    Isolate n '(pCh, pEnd, s) = '(IsolateChSym pCh pEnd, IsolateEndSym, '(n, s))

type IsolateCh
    :: ParserChSym s r
    -> ParserEndSym s r
    -> ParserCh (Natural, s) r
type family IsolateCh pCh pEnd ch s where
    IsolateCh pCh pEnd ch '(0, s) =
        Err (Text "cannot isolate 0 due to parser limitations")
    IsolateCh pCh pEnd ch '(1, s) = IsolateInnerEnd' pEnd (pCh @@ ch @@ s)
    IsolateCh pCh pEnd ch '(n, s) = IsolateInner n (pCh @@ ch @@ s)

-- TODO clean up names here

--type IsolateInnerEnd' :: Either ErrorMessage r -> Result (Natural, s) r
type family IsolateInnerEnd' pEnd res where
    IsolateInnerEnd' pEnd (Err  e) = Err  e
    IsolateInnerEnd' pEnd (Done r) = Done r
    IsolateInnerEnd' pEnd (Cont s) = IsolateInnerEnd (pEnd @@ s)

type IsolateInnerEnd :: Either ErrorMessage r -> Result (Natural, s) r
type family IsolateInnerEnd a where
    IsolateInnerEnd (Left  e) = Err  e
    IsolateInnerEnd (Right r) = Done r

type IsolateInner :: Natural -> Result s r -> Result (Natural, s) r
type family IsolateInner n a where
    IsolateInner _ (Err  e) = Err  e
    IsolateInner _ (Done _) =
        -- TODO put n in that error too plz
        Err (Text "isolated parser ended without consuming all input")
    IsolateInner n (Cont s) = Cont '(n-1, s)

type IsolateEnd :: ParserEnd (Natural, s) r
type family IsolateEnd s where
    IsolateEnd '(0, s) = Right '(0, s)
    IsolateEnd '(n, s) =
        -- TODO
        Left (Text "isolate wanted more than was there")

type IsolateChSym
    :: ParserChSym s r
    -> ParserEndSym s r
    -> ParserChSym (Natural, s) r
data IsolateChSym pCh pEnd f
type instance App (IsolateChSym pCh pEnd) f = IsolateChSym1 pCh pEnd f

type IsolateChSym1
    :: ParserChSym s r
    -> ParserEndSym s r
    -> Char -> (Natural, s) ~> Result (Natural, s) r
data IsolateChSym1 pCh pEnd ch s
type instance App (IsolateChSym1 pCh pEnd ch) s = IsolateCh pCh pEnd ch s

type IsolateEndSym :: ParserEndSym (Natural, s) r
data IsolateEndSym s
type instance App IsolateEndSym s = IsolateEnd s
