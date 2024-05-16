module Symparsec.Util where

import GHC.TypeLits

-- | Re-construct the output from 'UnconsSymbol'.
type family ReconsSymbol msym where
    ReconsSymbol Nothing           = ""
    ReconsSymbol (Just '(ch, sym)) = ConsSymbol ch sym
