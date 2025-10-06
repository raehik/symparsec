{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Literal where

import Symparsec2.Parser.Common
import Symparsec2.Utils ( type IfNatLte )
import Data.Type.Symbol qualified as Symbol

-- TODO megaparsec auto-backtracks its similar primitive.
-- confusingly they write it on the type class method Haddock, even though it
-- doesn't seem enforced (the backtracking is done internally).
-- idk why. but should we also?
-- (similarly to megaparsec, it doesn't really change performance.)

type EDuringLit :: Symbol -> Symbol -> PError
type EDuringLit lit detail = 'Error
    [ "while parsing literal '" ++ lit ++ "':"
    , detail ]

type ETooShort lit nNeed nGot =
    EDuringLit lit (EStrInputTooShort nNeed nGot)

type EWrongChar lit chExpect chGot =
    EDuringLit lit (EStrWrongChar chExpect chGot)

type EEof lit = EDuringLit lit "EOF while still parsing literal"

type Literal :: Symbol -> PParser ()
data Literal lit s
type instance App (Literal lit) s = LiteralCheckLen lit s (Symbol.Length lit)

type family LiteralCheckLen lit s n where
    LiteralCheckLen lit ('State rem len idx) litLen =
        IfNatLte litLen len
            (LiteralStep lit ('State rem len idx))
            ('Reply (Err (ETooShort lit litLen len)) ('State rem len idx))

type LiteralStep lit s = Literal' lit s (UnconsSymbol lit) (UnconsState s)
type family Literal' lit sPrev ch ms where
    Literal'  lit sPrev (Just '(litCh, litStr)) '(Just litCh,  s) =
        Literal' lit s (UnconsSymbol litStr) (UnconsState s)
    Literal' _lit sPrev Nothing                 _                 =
        'Reply (OK '()) sPrev
    Literal'  lit sPrev (Just '(litCh, litStr)) '(Just    ch, _s) =
        -- TODO which state to pass back here?
        'Reply (Err (EWrongChar lit litCh ch)) sPrev
    Literal'  lit sPrev (Just '(litCh, litStr)) '(Nothing,    _s) =
        -- note that this equation is impossible providing length is checked
        -- both states are guaranteed the same here, but prev is morally better
        'Reply (Err (EEof lit)) sPrev
