{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Isolate where -- ( Isolate ) where

import Symparsec.Parser.Common
import GHC.TypeLits ( Natural, type (-) )
import TypeLevelShow.Natural ( ShowNatDec )

-- | Run the given parser isolated to the next @n@ characters.
--
-- All isolated characters must be consumed.
type Isolate :: Natural -> ParserSym s r -> ParserSym (Natural, s) r
type family Isolate n p where
    Isolate 0 ('ParserSym pCh pEnd s) = 'ParserSym
        (FailChSym "Isolate" (ErrParserLimitation "cannot isolate 0"))
        IsolateEndSym '(0, s)
    Isolate n p = Isolate' n p

-- | unsafe, doesn't check for bad stuck behaviour
type Isolate' :: Natural -> ParserSym s r -> ParserSym (Natural, s) r
type family Isolate' n p where
    Isolate' n ('ParserSym pCh pEnd s) = Isolate'' n pCh pEnd s

-- | unsafe, and unwrapped for permitting instances
type Isolate''
    :: Natural
    -> ParserChSym s r -> ParserEndSym s r -> s
    -> ParserSym (Natural, s) r
type Isolate'' n pCh pEnd s =
    'ParserSym (IsolateChSym pCh pEnd) IsolateEndSym '(n, s)

type IsolateCh
    :: ParserChSym s r
    -> ParserEndSym s r
    -> PParserCh (Natural, s) r
type family IsolateCh pCh pEnd ch s where
    IsolateCh pCh pEnd ch '(1, s) = IsolateInnerEnd' pEnd (pCh @@ ch @@ s)
    IsolateCh pCh pEnd ch '(n, s) = IsolateInner n (pCh @@ ch @@ s)

-- TODO clean up names here

--type IsolateInnerEnd' :: Either ErrorMessage r -> Result (Natural, s) r
type family IsolateInnerEnd' pEnd res where
    IsolateInnerEnd' pEnd (Err  e) = Err  (EIn "Isolate" e)
    IsolateInnerEnd' pEnd (Done r) = Done r
    IsolateInnerEnd' pEnd (Cont s) = IsolateInnerEnd (pEnd @@ s)

type IsolateInnerEnd :: Either PE r -> PResult (Natural, s) r
type family IsolateInnerEnd a where
    IsolateInnerEnd (Left  e) = Err  (EIn "Isolate" e)
    IsolateInnerEnd (Right r) = Done r

type IsolateInner :: Natural -> PResult s r -> PResult (Natural, s) r
type family IsolateInner n a where
    IsolateInner n (Cont s) = Cont '(n-1, s)
    IsolateInner _ (Err  e) = Err  e
    IsolateInner n (Done _) = Err (EBase "Isolate"
        (      Text "isolated parser ended without consuming all input ("
          :<>: Text (ShowNatDec n) :<>: Text " remaining)" ))

type IsolateEnd :: PParserEnd (Natural, s) r
type family IsolateEnd s where
    IsolateEnd '(0, s) = Right '(0, s)
    IsolateEnd '(n, s) = Left (EBase "Isolate"
        (      Text "tried to isolate more than present (needed "
          :<>: Text (ShowNatDec n) :<>: Text " more)" ))

type IsolateChSym
    :: ParserChSym s r
    -> ParserEndSym s r
    -> ParserChSym (Natural, s) r
data IsolateChSym pCh pEnd f
type instance App (IsolateChSym pCh pEnd) f = IsolateChSym1 pCh pEnd f

type IsolateChSym1
    :: ParserChSym s r
    -> ParserEndSym s r
    -> Char -> (Natural, s) ~> PResult (Natural, s) r
data IsolateChSym1 pCh pEnd ch s
type instance App (IsolateChSym1 pCh pEnd ch) s = IsolateCh pCh pEnd ch s

type IsolateEndSym :: ParserEndSym (Natural, s) r
data IsolateEndSym s
type instance App IsolateEndSym s = IsolateEnd s
