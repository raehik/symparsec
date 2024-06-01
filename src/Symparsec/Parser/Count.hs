{-# LANGUAGE UndecidableInstances #-}

-- TODO not using end parser yet. maybe don't in ch parser?

module Symparsec.Parser.Count where

import Symparsec.Parser.Common
import GHC.TypeLits hiding ( ErrorMessage(..) )
import Singleraeh.Tuple
import Singleraeh.List
import Singleraeh.Either
import Singleraeh.Natural
import DeFun.Core
import Data.Type.Equality
import Unsafe.Coerce ( unsafeCoerce )

type CountS s r = (Natural, [r], s)

type family Count n p where
    Count n ('PParser pCh pEnd s0) = Count' n pCh pEnd s0
type Count' n pCh pEnd s0 =
    'PParser (CountChSym pCh s0) (CountEndSym pEnd s0) '(n, '[], s0)

type SCountS ss sr = STuple3 SNat (SList sr) ss

sCount
    :: SNat n
    -> SParser ss sr ('PParser pCh pEnd s0)
    -> SParser (SCountS ss sr) (SList sr) (Count' n pCh pEnd s0)
sCount n (SParser pCh pEnd s0) =
    SParser (sCountChSym pCh s0) (sCountEndSym pEnd s0) (STuple3 n SNil s0)

instance
  ( p ~ 'PParser pCh pEnd s0, SingParser p, KnownNat n
  ) => SingParser (Count' n pCh pEnd s0) where
    type PS (Count' n pCh pEnd s0) =
          SCountS (PS ('PParser pCh pEnd s0)) (PR ('PParser pCh pEnd s0))
    type PR (Count' n pCh pEnd s0) = SList (PR ('PParser pCh pEnd s0))
    singParser' = sCount SNat (singParser @p)

type family CountCh pCh s0 ch s where
    CountCh pCh s0 ch '(n, rs, s) = CountCh' pCh s0 ch n rs s

type family CountCh' pCh s0 ch n rs s where
    CountCh' pCh s0 ch 0 rs s = Done (Reverse rs)
    CountCh' pCh s0 ch n rs s = CountChN pCh ch n rs s0 (pCh @@ ch @@ s)

type family CountChN pCh ch n rs s0 res where
    CountChN pCh ch n rs s0 (Cont s) = Cont '(n, rs, s)
    CountChN pCh ch n rs s0 (Done r) = CountCh' pCh s0 ch (n-1) (r:rs) s0
    CountChN pCh ch n rs s0 (Err  e) = Err (ECount e)

{-
sCountChN
    :: SNat n
    -> SList sr rs
    -> ss s0
    -> SResult ss sr res
    -> SResult (SCountS ss sr) (SList sr) (CountChN n rs s0 res)
sCountChN n rs s0 = \case
  SCont s -> SCont $ STuple3 n           rs           s
  SDone r -> SCont $ STuple3 (n %- SNat) (SCons r rs) s0
  SErr  e -> SErr  $ eCount e
-}

type ECount e = EIn "Count" e
eCount :: SE e -> SE (ECount e)
eCount e = withSingE e $ singE

type CountChSym
    :: ParserChSym  s r
    -> s
    -> ParserChSym  (CountS s r) [r]
data CountChSym pCh s0 f
type instance App (CountChSym pCh s0) f = CountChSym1 pCh s0 f

type CountChSym1
    :: ParserChSym  s r
    -> s
    -> ParserChSym1 (CountS s r) [r]
data CountChSym1 pCh s0 ch s
type instance App (CountChSym1 pCh s0 ch) s = CountCh pCh s0 ch s

sCountChSym
    :: SParserChSym  ss sr pCh
    -> ss s0
    -> SParserChSym (SCountS ss sr) (SList sr) (CountChSym pCh s0)
sCountChSym pCh s0 = Lam2 $ \ch (STuple3 n rs s) ->
    case testEquality n (SNat @0) of
      Just Refl -> SDone $ sReverse rs
      -- TODO cba..................
      Nothing   -> error "not implemented" -- unsafeCoerce $ sCountChN n rs s0 (pCh @@ ch @@ s)

type family CountEnd pEnd s0 s where
    CountEnd pEnd s0 '(n, rs, s) = CountEnd' pEnd s0 n rs s

type family CountEnd' pEnd s0 n rs s where
    CountEnd' pEnd s0 0 rs s = Right (Reverse rs)
    CountEnd' pEnd s0 n rs s = CountEndN pEnd s0 n rs (pEnd @@ s)

sCountEnd'
    :: SParserEndSym ss sr pEnd
    -> ss s0
    -> SNat n
    -> SList sr rs
    -> ss s
    -> SResultEnd (SList sr) (CountEnd' pEnd s0 n rs s)
sCountEnd' pEnd s0 n rs s =
    case testEquality n (SNat @0) of
      Just Refl -> SRight $ sReverse rs
      Nothing   -> unsafeCoerce $ sCountEndN pEnd s0 n rs (pEnd @@ s)

type family CountEndN pEnd s0 n rs res where
    CountEndN pEnd s0 n rs (Right r) = CountEnd' pEnd s0 (n-1) (r:rs) s0
    CountEndN pEnd s0 n rs (Left  e) = Left (ECount e)

sCountEndN
    :: SParserEndSym ss sr pEnd
    -> ss s0
    -> SNat n
    -> SList sr rs
    -> SResultEnd sr res
    -> SResultEnd (SList sr) (CountEndN pEnd s0 n rs res)
sCountEndN pEnd s0 n rs = \case
  SRight r -> sCountEnd' pEnd s0 (n %- SNat @1) (SCons r rs) s0
  SLeft  e -> SLeft $ eCount e

type CountEndSym
    :: ParserEndSym s r
    -> s
    -> ParserEndSym (CountS s r) [r]
data CountEndSym pEnd s0 s
type instance App (CountEndSym pEnd s0) s = CountEnd pEnd s0 s

sCountEndSym
    :: SParserEndSym ss              sr         pEnd
    -> ss s0
    -> SParserEndSym (SCountS ss sr) (SList sr) (CountEndSym pEnd s0)
sCountEndSym pEnd s0 = Lam $ \(STuple3 n rs s) -> sCountEnd' pEnd s0 n rs s
