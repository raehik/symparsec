{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Run ( Run ) where

import Data.Type.Symbol.Parser.Types
import GHC.TypeLits
import DeFun.Core ( type (@@) )

type family Run p sym where
    Run '(pCh, pEnd, s) sym =
        Run' pCh pEnd 0 s (UnconsSymbol sym)

-- TODO maybe take an mch? Nothing at start, Just otherwise
type family Run' pCh pEnd idx s msym where
    Run' pCh pEnd idx s (Just '(ch, sym)) =
        Run'' pCh pEnd idx ch (pCh @@ ch @@ s) sym
    Run' pCh pEnd idx s Nothing =
        RunEnd idx (pEnd @@ s)

type family RunEnd idx end where
    RunEnd idx (Left  e) = Left e
    RunEnd idx (Right r) = Right '(r, "")

type family Run'' pCh pEnd idx ch res sym where
    Run'' pCh pEnd idx ch (Err  e) sym = Left e -- TODO annotate error
    Run'' pCh pEnd idx ch (Done r) sym = Right '(r, sym)
    Run'' pCh pEnd idx ch (Cont s) sym =
        Run' pCh pEnd (idx+1) s (UnconsSymbol sym)
