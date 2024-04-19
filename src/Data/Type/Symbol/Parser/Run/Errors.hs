module Data.Type.Symbol.Parser.Run.Errors where

import Data.Type.Symbol.Parser.Types
import GHC.TypeError

type PrettyE :: E -> ErrorMessage
type family PrettyE e where
    PrettyE (E0 ep) = Text "parse error on empty string" :$$: PrettyEP ep
    PrettyE ('E idx ch ep) =
             Text "parse error at index " :<>: ShowType idx
        :<>: Text ", char " :<>: ShowType ch :$$: PrettyEP ep

type family PrettyEP ep where
    PrettyEP (EBase name e)  = Text name :<>: Text ": " :<>: e
    PrettyEP (EIn   name ep) = Text name :<>: Text ": " :<>: PrettyEP ep
