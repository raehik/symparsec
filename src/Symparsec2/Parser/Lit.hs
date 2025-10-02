{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Lit where

import Symparsec2.Parser.Common
import GHC.TypeLits ( Symbol, type UnconsSymbol, type ConsSymbol )
import TypeLevelShow.Utils ( type ShowChar )

type EWrongChar lit chExpect chGot = EBase "Lit"
    (      Text "while parsing literal '" :<>: Text lit
      :<>: Text "': expected '" :<>: Text (ShowChar chExpect)
      :<>: Text "', got '"   :<>: Text (ShowChar chGot) :<>: Text "'")

type EStillParsing lit rem = EBase "Lit"
    (      Text "while parsing literal '" :<>: Text lit
      :<>: Text "': end of input, still parsing '" :<>: Text rem :<>: Text "'")

type Lit :: Symbol -> PParserSym ()
data Lit lit s
type instance App (Lit lit) s = LitStep lit s
type LitStep lit s = Lit' lit s (UnconsSymbol lit) (UnconsState s)
type family Lit' lit sPrev ch ms where
    Lit'  lit sPrev (Just '(litCh, litStr)) '(Just litCh,  s) =
        Lit' lit s (UnconsSymbol litStr) (UnconsState s)
    Lit' _lit sPrev Nothing                 _                 =
        Done sPrev '()
    Lit'  lit sPrev (Just '(litCh, litStr)) '(Just    ch, _s) =
        -- TODO which state to pass back here?
        Err sPrev (EWrongChar lit litCh ch)
    Lit'  lit sPrev (Just '(litCh, litStr)) '(Nothing,    _s) =
        -- both states are guaranteed the same here, but prev is morally better
        Err sPrev (EStillParsing lit (ConsSymbol litCh litStr))
