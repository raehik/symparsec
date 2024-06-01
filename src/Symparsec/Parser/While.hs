{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.While where

import Symparsec.Parser.Common
import DeFun.Core
import GHC.TypeLits
import Singleraeh.Bool
import Singleraeh.Either
import Symparsec.Parser.While.Predicates

-- | Run the given parser while the given character predicate succeeds.
type family While chPred p where
    While chPred ('PParser pCh pEnd s0) = While' chPred pCh pEnd s0
type While' chPred pCh pEnd s0 = 'PParser (WhileChSym chPred pCh pEnd) pEnd s0

sWhile
    :: Lam SChar SBool chPred
    -> SParser ss sr ('PParser pCh pEnd s0)
    -> SParser ss sr (While' chPred pCh pEnd s0)
sWhile chPred (SParser pCh pEnd s0) =
    SParser (sWhileChSym chPred pCh pEnd) pEnd s0

instance
  ( p ~ 'PParser pCh pEnd s0, SingParser p
  , SingChPred chPred
  ) => SingParser (While' chPred pCh pEnd s0) where
    type PS (While' chPred pCh pEnd s0) = PS ('PParser pCh pEnd s0)
    type PR (While' chPred pCh pEnd s0) = PR ('PParser pCh pEnd s0)
    singParser' = sWhile (singChPred @chPred) (singParser @p)

type WhileCh chPred pCh pEnd ch s = WhileCh' pCh pEnd ch s (chPred @@ ch)
type family WhileCh' pCh pEnd ch s res where
    WhileCh' pCh pEnd ch s True  = pCh @@ ch @@ s
    WhileCh' pCh pEnd ch s False = WhileCh'' (pEnd @@ s)

type family WhileCh'' res where
    WhileCh'' (Right r) = Done r
    WhileCh'' (Left  e) = Err (EWhile e)

type EWhile e = EIn "While" e
eWhile :: SE e -> SE (EWhile e)
eWhile e = withSingE e $ singE

type WhileChSym
    :: (Char ~> Bool)
    -> ParserChSym  s r
    -> ParserEndSym s r
    -> ParserChSym  s r
data WhileChSym chPred pCh pEnd f
type instance App (WhileChSym chPred pCh pEnd) f = WhileChSym1 chPred pCh pEnd f

type WhileChSym1
    :: (Char ~> Bool)
    -> ParserChSym  s r
    -> ParserEndSym s r
    -> ParserChSym1 s r
data WhileChSym1 chPred pCh pEnd ch s
type instance App (WhileChSym1 chPred pCh pEnd ch) s = WhileCh chPred pCh pEnd ch s

sWhileChSym
    :: Lam SChar SBool chPred
    -> SParserChSym  ss sr pCh
    -> SParserEndSym ss sr pEnd
    -> SParserChSym  ss sr (WhileChSym chPred pCh pEnd)
sWhileChSym chPred pCh pEnd = Lam2 $ \ch s ->
    case chPred @@ ch of
      STrue  -> pCh @@ ch @@ s
      SFalse ->
        case pEnd @@ s of
          SRight r -> SDone r
          SLeft  e -> SErr $ eWhile e
