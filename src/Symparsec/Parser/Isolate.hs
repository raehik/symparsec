{-# LANGUAGE UndecidableInstances #-}

-- TODO complex parser with weird edge cases. needs clean up & tests

module Symparsec.Parser.Isolate ( Isolate, Isolate', Isolate'' ) where

import Symparsec.Parser.Common
import GHC.TypeLits ( Natural, type (-) )
import TypeLevelShow.Natural ( ShowNatDec )

-- | Run the given parser isolated to the next @n@ characters.
--
-- All isolated characters must be consumed.
type Isolate :: Natural -> Parser s r -> Parser (Natural, s) r
type family Isolate n p where
    Isolate 0 ('Parser pCh pEnd s) = 'Parser
        (FailChSym "Isolate" (ErrParserLimitation "cannot isolate 0"))
        (IsolateEndSym pEnd) '(0, s)
    Isolate n p = Isolate' n p

-- | unsafe, doesn't check for bad stuck behaviour
type Isolate' :: Natural -> Parser s r -> Parser (Natural, s) r
type family Isolate' n p where
    Isolate' n ('Parser pCh pEnd s) = Isolate'' n pCh pEnd s

-- | unsafe, and unwrapped for permitting instances
type Isolate''
    :: Natural
    -> ParserChSym s r -> ParserEndSym s r -> s
    -> Parser (Natural, s) r
type Isolate'' n pCh pEnd s =
    'Parser (IsolateChSym pCh pEnd) (IsolateEndSym pEnd) '(n, s)

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

type IsolateEnd :: ParserEndSym s r -> PParserEnd (Natural, s) r
type family IsolateEnd pEnd s where
    IsolateEnd pEnd '(0, s) = IsolateEnd' (pEnd @@ s)
    -- ^ will only occur on @Isolate 0@
    IsolateEnd pEnd '(n, s) = Left (EBase "Isolate"
        (      Text "tried to isolate more than present (needed "
          :<>: Text (ShowNatDec n) :<>: Text " more)" ))

--type IsolateEnd' :: Result s r -> Result (PParserEnd (Natural, s) r
type family IsolateEnd' res where
    IsolateEnd' (Right r) = Right r
    IsolateEnd' (Left  e) = Left (EIn "Isolate" e)

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

type IsolateEndSym :: ParserEndSym s r -> ParserEndSym (Natural, s) r
data IsolateEndSym pEnd s
type instance App (IsolateEndSym pEnd) s = IsolateEnd pEnd s
