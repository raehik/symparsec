{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Internal where

import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@) )

type Parser s r = Char -> s -> Result s r
data Result s r = Cont s | Done r | Err ErrorMessage

type ParserEnd s r = s -> Either ErrorMessage r

type ParserSym s r = Char ~> s ~> Result s r
type ParserEndSym s r = s ~> Either ErrorMessage r

type ParserSym' s r = (ParserSym s r, ParserEndSym s r, s)

type family RunParser p sym where
    RunParser '(pCh, pEnd, s) sym =
        RunParser' pCh pEnd 0 s (UnconsSymbol sym)

-- TODO maybe take an mch? Nothing at start, Just otherwise
type family RunParser' pCh pEnd idx s msym where
    RunParser' pCh pEnd idx s 'Nothing =
        RunParserEnd idx (pEnd @@ s)
    RunParser' pCh pEnd idx s ('Just '(ch, sym)) =
        RunParser'' pCh pEnd idx ch (pCh @@ ch @@ s) sym

type family RunParserEnd idx end where
    RunParserEnd idx ('Left  e) = 'Left e
    RunParserEnd idx ('Right r) = 'Right '(r, "")

type family RunParser'' pCh pEnd idx ch res sym where
    RunParser'' pCh pEnd idx ch ('Err  e) sym = 'Left e -- TODO annotate error
    RunParser'' pCh pEnd idx ch ('Done r) sym = 'Right '(r, sym)
    RunParser'' pCh pEnd idx ch ('Cont s) sym =
        RunParser' pCh pEnd (idx+1) s (UnconsSymbol sym)

-- TODO could do this if more parsers end up storing state which they emit
-- precisely (NatBase does this)
--type ParserEndEmit :: ParserEnd r r
