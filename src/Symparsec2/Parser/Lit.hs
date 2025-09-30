{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Lit where

import Symparsec2.Parser.Common
import GHC.TypeLits ( Symbol, type UnconsSymbol, type ConsSymbol )
import TypeLevelShow.Utils ( type ShowChar )

type EWrongChar chExpect chGot = EBase "Lit"
    (      Text "expected" :<>: Text (ShowChar chExpect)
      :<>: Text ", got"    :<>: Text (ShowChar chGot))

type EStillParsing str =
    EBase "Lit" (Text "still parsing literal: " :<>: Text str)

type Lit :: Symbol -> PParserSym ()
data Lit lit s
type instance App (Lit lit) s = LitStep lit s
type LitStep lit s = Lit' s (UnconsSymbol lit) (UnconsState s)
type family Lit' sPrev ch ms where
    Lit' sPrev (Just '(litCh, litStr)) '(Just litCh, s) =
        LitStep litStr s
    Lit' sPrev Nothing                 _                =
        Done sPrev '()
    Lit' sPrev (Just '(litCh, litStr)) '(Just    ch, s) =
        -- TODO which state to pass back here?
        Err sPrev (EWrongChar litCh ch)
    Lit' sPrev (Just '(litCh, litStr)) '(Nothing,    s) =
        -- TODO which state to pass back here?
        Err sPrev (EStillParsing (ConsSymbol litCh litStr))
