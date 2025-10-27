{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.TakeWhile ( type TakeWhile, type TakeWhile1 ) where

import Symparsec.Parser.Common
import Singleraeh.Symbol ( type RevCharsToSymbol )

-- | Take zero or more 'Char's for which the supplied predicate holds.
--
-- May also be defined via
-- @'Symparsec.Parser.While.While' chPred 'Symparsec.Parser.TakeRest.TakeRest'@,
-- but a custom implementation is more efficient.
type TakeWhile :: (Char ~> Bool) -> PParser s Symbol
data TakeWhile chPred ps
type instance App (TakeWhile chPred) ps = TakeWhileStart chPred ps (UnconsState ps)

type family TakeWhileStart chPred psPrev mps where
    TakeWhileStart chPred psPrev '(Just ch, ps) =
        TakeWhileLoop chPred psPrev ps ch '[] (chPred @@ ch) (UnconsState ps)
    TakeWhileStart chPred psPrev '(Nothing, ps) =
        'Reply (OK "") psPrev

type family TakeWhileLoop chPred psPrev psCh ch taken res mps where
    -- next char succeeded and not EOF
    TakeWhileLoop chPred psPrev psCh ch taken True '(Just chNext, ps) =
        TakeWhileLoop chPred psCh ps chNext (ch:taken) (chPred @@ chNext) (UnconsState ps)

    -- next char succeeded and EOF: end
    TakeWhileLoop chPred sPrev psCh ch taken True '(Nothing, ps) =
        'Reply (OK (RevCharsToSymbol (ch:taken))) psCh -- @psCh == ps@ should hold

    -- next char failed: backtrack and end
    TakeWhileLoop chPred psPrev psCh ch taken False _ =
        'Reply (OK (RevCharsToSymbol taken)) psPrev

-- | Take one or more 'Char's for which the supplied predicate holds.
--
-- Backtracks on failure. Same as megaparsec.
type TakeWhile1 :: (Char ~> Bool) -> PParser s Symbol
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
