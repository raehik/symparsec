{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.TakeWhile ( type TakeWhile, type TakeWhile1 ) where

import Symparsec.Parser.Common
import Singleraeh.Symbol ( type RevCharsToSymbol )

-- | Take zero or more 'Char's for which the supplied predicate holds.
--
-- May also be defined via
-- @'Symparsec.Parser.While.While' chPred 'Symparsec.Parser.TakeRest.TakeRest'@,
-- but a custom implementation is more efficient.
type TakeWhile :: (Char ~> Bool) -> PParser u Symbol
data TakeWhile chPred s
type instance App (TakeWhile chPred) s = TakeWhileStart chPred s (UnconsState s)

type family TakeWhileStart chPred sPrev ms where
    TakeWhileStart chPred sPrev '(Just ch, s) =
        TakeWhileLoop chPred sPrev s ch '[] (chPred @@ ch) (UnconsState s)
    TakeWhileStart chPred sPrev '(Nothing, s) =
        'Reply (OK "") sPrev

type family TakeWhileLoop chPred sPrev sCh ch taken res ms where
    -- next char succeeded and not EOF
    TakeWhileLoop chPred sPrev sCh ch taken True '(Just chNext, s) =
        TakeWhileLoop chPred sCh s chNext (ch:taken) (chPred @@ chNext) (UnconsState s)

    -- next char succeeded and EOF: end
    TakeWhileLoop chPred sPrev sCh ch taken True '(Nothing, s) =
        'Reply (OK (RevCharsToSymbol (ch:taken))) sCh -- @sCh == s@ should hold

    -- next char failed: backtrack and end
    TakeWhileLoop chPred sPrev sCh ch taken False _ =
        'Reply (OK (RevCharsToSymbol taken)) sPrev

-- | Take one or more 'Char's for which the supplied predicate holds.
--
-- Backtracks on failure. Same as megaparsec.
type TakeWhile1 :: (Char ~> Bool) -> PParser u Symbol
data TakeWhile1 chPred ps
type instance App (TakeWhile1 chPred) ps = TakeWhile1Start chPred ps (UnconsState ps)

type family TakeWhile1Start chPred psPrev mps where
    TakeWhile1Start chPred psPrev '(Just ch, ps) =
        TakeWhile1Start2 chPred psPrev ps ch (chPred @@ ch) (UnconsState ps)
    TakeWhile1Start chPred psPrev '(Nothing, ps) =
        'Reply (Err (Error1 "empty string")) psPrev

type family TakeWhile1Start2 chPred psPrev ps ch res mps where
    TakeWhile1Start2 chPred psPrev psCh ch True  '(Just chNext, ps) =
        TakeWhileLoop chPred psCh ps chNext '[ch] (chPred @@ chNext) (UnconsState ps)
    TakeWhile1Start2 chPred psPrev psCh ch True  '(Nothing,     ps) =
        'Reply (OK (ConsSymbol ch "")) ps
    TakeWhile1Start2 chPred psPrev psCh ch False _ =
        'Reply (Err (Error1 "TakeWhile1 didn't even get 1 char")) psPrev
