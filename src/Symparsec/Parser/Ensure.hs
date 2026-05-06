{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Ensure ( type Ensure ) where

import Symparsec.Parser.Common
import Symparsec.Utils ( type IfNatLte )

-- | Assert that there are at least @n@ characters remaining. Non-consuming.
type Ensure :: Natural -> PParser u ()
data Ensure n s
type instance App (Ensure n) s = Ensure' n s
type family Ensure' n s where
    Ensure' n ('State rem len idx user) =
        IfNatLte n len
            ('Reply (OK '()) ('State rem len idx user))
            ('Reply (Err (Error1 (EStrInputTooShort n len))) ('State rem len idx user))
