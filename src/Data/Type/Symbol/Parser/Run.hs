{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Run ( RunParser ) where

import Data.Type.Symbol.Parser.Types
import GHC.TypeLits
import DeFun.Core ( type (@@) )

type family RunParser p sym where
    RunParser '(pCh, pEnd, s) sym =
        RunParser' pCh pEnd 0 s (UnconsSymbol sym)

-- TODO maybe take an mch? Nothing at start, Just otherwise
type family RunParser' pCh pEnd idx s msym where
    RunParser' pCh pEnd idx s Nothing =
        RunParserEnd idx (pEnd @@ s)
    RunParser' pCh pEnd idx s (Just '(ch, sym)) =
        RunParser'' pCh pEnd idx ch (pCh @@ ch @@ s) sym

type family RunParserEnd idx end where
    RunParserEnd idx (Left  e) = Left e
    RunParserEnd idx (Right r) = Right '(r, "")

type family RunParser'' pCh pEnd idx ch res sym where
    RunParser'' pCh pEnd idx ch (Err  e) sym = Left e -- TODO annotate error
    RunParser'' pCh pEnd idx ch (Done r) sym = Right '(r, sym)
    RunParser'' pCh pEnd idx ch (Cont s) sym =
        RunParser' pCh pEnd (idx+1) s (UnconsSymbol sym)
