{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Isolate where

import Data.Type.Symbol.Parser.Internal
import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@), type App )

-- TODO have to pass init state here too awkwardly :/
type PIsolate
    :: Natural -> ParserSym' s r -> ParserSym' (Natural, s) r
type family PIsolate n p where
    PIsolate n '(pCh, pEnd, s) = '(IsolateSym pCh pEnd, IsolateEndSym, '(n, s))

--type Isolate :: ParserSym' s r -> Parser (Natural, s) r
type family Isolate pCh pEnd ch s where
    Isolate pCh pEnd ch '(0, s) =
        'Err ('Text "cannot isolate 0 due to parser limitations")
    Isolate pCh pEnd ch '(1, s) = IsolateInnerEnd' pEnd (pCh @@ ch @@ s)
    Isolate pCh pEnd ch '(n, s) = IsolateInner n (pCh @@ ch @@ s)

--type IsolateInnerEnd' :: Either ErrorMessage r -> Result (Natural, s) r
type family IsolateInnerEnd' pEnd res where
    IsolateInnerEnd' pEnd ('Err  e) = 'Err  e
    IsolateInnerEnd' pEnd ('Done r) = 'Done r
    IsolateInnerEnd' pEnd ('Cont s) = IsolateInnerEnd (pEnd @@ s)

type IsolateInnerEnd :: Either ErrorMessage r -> Result (Natural, s) r
type family IsolateInnerEnd a where
    IsolateInnerEnd ('Left  e) = 'Err  e
    IsolateInnerEnd ('Right r) = 'Done r

type IsolateInner :: Natural -> Result s r -> Result (Natural, s) r
type family IsolateInner n a where
    IsolateInner _ ('Err  e) = 'Err  e
    IsolateInner _ ('Done _) =
        -- TODO put n in that error too plz
        'Err ('Text "isolated parser ended without consuming all input")
    IsolateInner n ('Cont s) = 'Cont '(n-1, s)

type IsolateEnd :: ParserEnd (Natural, s) r
type family IsolateEnd s where
    IsolateEnd '(0, s) = 'Right '(0, s)
    IsolateEnd '(n, s) =
        -- TODO
        'Left ('Text "isolate wanted more than was there")

type IsolateSym
    :: ParserSym s r -> ParserEndSym s r
    -> ParserSym (Natural, s) r
data IsolateSym pCh pEnd f
type instance App (IsolateSym pCh pEnd) f = IsolateSym1 pCh pEnd f

type IsolateSym1
    :: ParserSym s r -> ParserEndSym s r
    -> Char -> (Natural, s) ~> Result (Natural, s) r
data IsolateSym1 pCh pEnd ch s
type instance App (IsolateSym1 pCh pEnd ch) s = Isolate pCh pEnd ch s

type IsolateEndSym :: ParserEndSym (Natural, s) r
data IsolateEndSym s
type instance App IsolateEndSym s = IsolateEnd s
