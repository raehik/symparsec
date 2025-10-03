{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Literal where

import Symparsec2.Parser.Common

type EWrongChar lit chExpect chGot = EBase "Literal"
    (      Text "while parsing literal '" :<>: Text lit
      :<>: Text "': expected '" :<>: Text (ShowChar chExpect)
      :<>: Text "', got '"   :<>: Text (ShowChar chGot) :<>: Text "'")

type EStillParsing lit rem = EBase "Literal"
    (      Text "while parsing literal '" :<>: Text lit
      :<>: Text "': end of input, still parsing '" :<>: Text rem :<>: Text "'")

type Literal :: Symbol -> PParserSym ()
data Literal lit s
type instance App (Literal lit) s = LiteralStep lit s
type LiteralStep lit s = Literal' lit s (UnconsSymbol lit) (UnconsState s)
type family Literal' lit sPrev ch ms where
    Literal'  lit sPrev (Just '(litCh, litStr)) '(Just litCh,  s) =
        Literal' lit s (UnconsSymbol litStr) (UnconsState s)
    Literal' _lit sPrev Nothing                 _                 =
        Done sPrev '()
    Literal'  lit sPrev (Just '(litCh, litStr)) '(Just    ch, _s) =
        -- TODO which state to pass back here?
        Err sPrev (EWrongChar lit litCh ch)
    Literal'  lit sPrev (Just '(litCh, litStr)) '(Nothing,    _s) =
        -- both states are guaranteed the same here, but prev is morally better
        Err sPrev (EStillParsing lit (ConsSymbol litCh litStr))
