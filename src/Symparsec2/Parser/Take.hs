{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Take where

import Symparsec2.Parser.Common
import Singleraeh.Symbol ( type RevCharsToSymbol )

-- | Return the next @n@ characters.
type Take :: Natural -> PParserSym Symbol
data Take n s
type instance App (Take n) s = Take' '[] n s (UnconsState s)
type family Take' chs n sPrev s where
    Take' chs 0 sPrev _             = Done sPrev (RevCharsToSymbol chs)
    Take' chs n sPrev '(Just ch, s) = Take' (ch:chs) (n-1) s (UnconsState s)
    Take' chs n sPrev '(Nothing, s) = Err sPrev (ETakeEnd n)
type ETakeEnd n = EBase "Take"
    (      Text "tried to take "
      :<>: Text (ShowNatDec n) :<>: Text " chars from empty string")
