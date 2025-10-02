module Symparsec2.Utils where

import GHC.TypeNats ( type CmpNat )
import Data.Type.Ord ( type OrdCond )

type IfNatLte n m fThen fElse = OrdCond (CmpNat n m) fThen fThen fElse
