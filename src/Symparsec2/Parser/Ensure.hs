{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Ensure ( type Ensure ) where

import Symparsec2.Parser.Common
import Symparsec2.Utils ( type IfNatLte )

-- | Assert that there are at least @n@ characters remaining. Non-consuming.
type Ensure :: Natural -> PParserSym ()
data Ensure n s
type instance App (Ensure n) s = Ensure' n s
type family Ensure' n s where
    Ensure' n ('State rem len idx) =
        IfNatLte n len
            ('Reply (OK '()) ('State rem len idx))
            ('Reply (Err (Error1 (EStrInputTooShort n len))) ('State rem len idx))
