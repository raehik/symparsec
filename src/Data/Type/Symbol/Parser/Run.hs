{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Run ( Run ) where

import Data.Type.Symbol.Parser.Types
import GHC.TypeLits
import DeFun.Core ( type (@@) )

-- | Run the given parser on the given 'Symbol'.
type Run :: Parser s r -> Symbol -> Either ErrorMessage (r, Symbol)
type family Run p sym where
    Run '(pCh, pEnd, s) sym =
        MapLeftPrettyERun (RunStart pCh pEnd s (UnconsSymbol sym))

type family RunStart pCh pEnd s msym where
    -- | Parsing non-empty string: call main loop
    RunStart pCh pEnd s (Just '(ch, sym)) =
        RunCh pCh pEnd 0 ch (UnconsSymbol sym) (pCh @@ ch @@ s)

    -- | Parsing empty string: call special early exit
    RunStart pCh pEnd s Nothing           = RunEnd0 (pEnd @@ s)

-- | Inspect character parser result.
--
-- This is purposely written so that the main case is at the top, and a single
-- equation (we parse, prepare next character and inspect character parser
-- result at the same time). My hope is that this keeps GHC fast.
type family RunCh pCh pEnd idx ch' msym res where
    -- | OK, and more to come: parse next character
    RunCh pCh pEnd idx ch' (Just '(ch, sym)) (Cont s) =
        RunCh pCh pEnd (idx+1) ch (UnconsSymbol sym) (pCh @@ ch @@ s)

    -- | OK, and we're at the end of the string: run end parser
    RunCh pCh pEnd idx ch' Nothing           (Cont s) =
        RunEnd idx ch' (pEnd @@ s)

    -- | OK, and we're finished early: return value and remaining string
    RunCh pCh pEnd idx ch' msym              (Done r) =
        Right '(r, ReconsSymbol msym)

    -- | Parse error: return error
    RunCh pCh pEnd idx ch' msym              (Err  e) =
        Left ('ERun idx ch' e)

-- | Inspect end parser result.
type RunEnd :: Natural -> Char -> Either E r -> Either ERun (r, Symbol)
type family RunEnd idx ch res where
    RunEnd idx ch (Right r) = Right '(r, "")
    RunEnd idx ch (Left  e) = Left ('ERun idx ch e)

-- | Inspect end parser result for the empty string, where we have no previous
--   character or (meaningful) index.
type family RunEnd0 res where
    RunEnd0 (Right r) = Right '(r, "")
    RunEnd0 (Left  e) = Left (ERun0 e)

type PrettyERun :: ERun -> ErrorMessage
type family PrettyERun e where
    PrettyERun (ERun0 e) = Text "parse error on empty string" :$$: PrettyE e
    PrettyERun ('ERun idx ch e) =
             Text "parse error at index " :<>: ShowType idx
        :<>: Text ", char " :<>: ShowType ch :$$: PrettyE e

type family PrettyE e where
    PrettyE (EBase name emsg)  = Text name :<>: Text ": " :<>: emsg
    PrettyE (EIn   name e) = Text name :<>: Text ": " :<>: PrettyE e

type family MapLeftPrettyERun eea where
    MapLeftPrettyERun (Left  e) = Left (PrettyERun e)
    MapLeftPrettyERun (Right a) = Right a

-- | Re-construct the output from 'UnconsSymbol'.
type family ReconsSymbol msym where
    ReconsSymbol Nothing           = ""
    ReconsSymbol (Just '(ch, sym)) = ConsSymbol ch sym
