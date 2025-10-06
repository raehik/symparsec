{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Take ( type Take, type TakeSym ) where

import Symparsec2.Parser.Common
import Singleraeh.Symbol ( type RevCharsToSymbol )

-- | Return the next @n@ characters.
type Take :: Natural -> PParser Symbol
data Take n s
type instance App (Take n) s = Take' '[] n s (UnconsState s)
type family Take' chs n sPrev s where
    Take' chs 0 sPrev _             = 'Reply (OK (RevCharsToSymbol chs)) sPrev
    Take' chs n sPrev '(Just ch, s) = Take' (ch:chs) (n-1) s (UnconsState s)
    Take' chs n sPrev '(Nothing, s) = 'Reply (Err (ETakeEnd n)) sPrev

type ETakeEnd :: Natural -> PError
type ETakeEnd n = Error1
    ( "tried to take " ++ ShowNatDec n ++ " chars from empty string" )

-- | 'Take' defunctionalization symbol.
type TakeSym :: Natural ~> PParser Symbol
data TakeSym n
type instance App TakeSym n = Take n
