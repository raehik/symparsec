{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.TakeWhile ( type TakeWhile ) where

import Symparsec.Parser.Common
import Singleraeh.Symbol ( type RevCharsToSymbol )

-- | Take zero or more 'Char's for which the supplied predicate holds.
--
-- May also be defined via
-- @'Symparsec.Parser.While.While' chPred 'Symparsec.Parser.TakeRest.TakeRest'@,
-- but a custom implementation is more efficient.
type TakeWhile :: (Char ~> Bool) -> PParser Symbol
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
