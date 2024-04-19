{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Run ( Run ) where

import Data.Type.Symbol.Parser.Types
import Data.Type.Symbol.Parser.Run.Errors
import GHC.TypeLits
import DeFun.Core ( type (@@) )

type Run :: Parser s r -> Symbol -> Either ErrorMessage (r, Symbol)
type family Run p sym where
    Run '(pCh, pEnd, s) sym =
        MapLeftPrettyE (RunStart pCh pEnd s (UnconsSymbol sym))

type family MapLeftPrettyE eea where
    MapLeftPrettyE (Left  e) = Left (PrettyE e)
    MapLeftPrettyE (Right a) = Right a

type family RunStart pCh pEnd s msym where
    RunStart pCh pEnd s (Just '(ch, sym)) =
        RunCh pCh pEnd 0 ch (UnconsSymbol sym) (pCh @@ ch @@ s)
    RunStart pCh pEnd s Nothing           = RunEnd0 (pEnd @@ s)

type family RunEnd0 res where
    RunEnd0 (Right r) = Right '(r, "")
    RunEnd0 (Left  e) = Left (E0 e)

type family RunCh pCh pEnd idx ch' sym res where
    RunCh pCh pEnd idx ch' (Just '(ch, sym)) (Cont s) =
        RunCh pCh pEnd (idx+1) ch (UnconsSymbol sym) (pCh @@ ch @@ s)
    RunCh pCh pEnd idx ch' Nothing           (Cont s) =
        RunEnd idx ch' (pEnd @@ s)
    RunCh pCh pEnd idx ch' msym              (Done r) =
        Right '(r, ReconsSymbol msym)
    RunCh pCh pEnd idx ch' msym              (Err  e) =
        Left ('E idx ch' e)
    --RunCh pCh pEnd idx ch' msym              x        =
     --   Left (ShowType x)

type family ReconsSymbol msym where
    ReconsSymbol Nothing           = ""
    ReconsSymbol (Just '(ch, sym)) = ConsSymbol ch sym

type family RunEnd idx ch res where
    RunEnd idx ch (Right r) = Right '(r, "")
    RunEnd idx ch (Left  e) = Left ('E idx ch e)
