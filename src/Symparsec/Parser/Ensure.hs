{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Ensure ( type Ensure ) where

import Symparsec.Parser.Common
import Symparsec.Utils ( type IfNatLte )

-- | Assert that there are at least @n@ characters remaining. Non-consuming.
type Ensure :: Natural -> PParser s ()
data Ensure n ps
type instance App (Ensure n) ps = Ensure' n ps
type family Ensure' n ps where
    Ensure' n ('State s rem len idx) =
        IfNatLte n len
            ('Reply (OK '()) ('State s rem len idx))
            ('Reply (Err (Error1 (EStrInputTooShort n len))) ('State s rem len idx))
