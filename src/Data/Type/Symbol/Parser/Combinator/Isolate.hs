{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Combinator.Isolate ( Isolate ) where

import Data.Type.Symbol.Parser.Types
import Data.Type.Symbol.Parser.Common
import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@), type App )

type Isolate :: Natural -> Parser s r -> Parser (Natural, s) r
type family Isolate n p where
    Isolate 0 '(pCh, pEnd, s) =
        '( FailChSym "Isolate" (ErrParserLimitation "cannot isolate 0")
         , IsolateEndSym, '(0, s))
    Isolate n '(pCh, pEnd, s) = '(IsolateChSym pCh pEnd, IsolateEndSym, '(n-1, s))

type IsolateCh
    :: ParserChSym s r
    -> ParserEndSym s r
    -> ParserCh (Natural, s) r
type family IsolateCh pCh pEnd ch s where
    IsolateCh pCh pEnd ch '(0, s) = IsolateInnerEnd' pEnd (pCh @@ ch @@ s)
    IsolateCh pCh pEnd ch '(n, s) = IsolateInner n (pCh @@ ch @@ s)

-- TODO clean up names here

--type IsolateInnerEnd' :: Either ErrorMessage r -> Result (Natural, s) r
type family IsolateInnerEnd' pEnd res where
    IsolateInnerEnd' pEnd (Err  e) = Err  (EIn "Isolate" e)
    IsolateInnerEnd' pEnd (Done r) = Done r
    IsolateInnerEnd' pEnd (Cont s) = IsolateInnerEnd (pEnd @@ s)

type IsolateInnerEnd :: Either EParser r -> Result (Natural, s) r
type family IsolateInnerEnd a where
    IsolateInnerEnd (Left  e) = Err  (EIn "Isolate" e)
    IsolateInnerEnd (Right r) = Done r

type IsolateInner :: Natural -> Result s r -> Result (Natural, s) r
type family IsolateInner n a where
    IsolateInner n (Cont s) = Cont '(n-1, s)
    IsolateInner _ (Err  e) = Err  e
    IsolateInner n (Done _) = Err (EBase "Isolate"
        (      Text "isolated parser ended without consuming all input ("
          :<>: ShowType n :<>: Text " remaining)" ))

type IsolateEnd :: ParserEnd (Natural, s) r
type family IsolateEnd s where
    IsolateEnd '(0, s) = Right '(0, s)
    IsolateEnd '(n, s) = Left (EBase "Isolate"
        (      Text "tried to isolate more than present (needed "
          :<>: ShowType n :<>: Text " more)" ))

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
