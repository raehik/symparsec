{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Take ( type Take, type TakeSym, type Take1 ) where

import Symparsec.Parser.Common
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

-- | Return the next character.
type Take1 :: PParser Char
data Take1 s
type instance App Take1 s = Take1' s (UnconsState s)
type family Take1' sPrev s where
    Take1' sPrev '(Just ch, s) = 'Reply (OK ch) s
    Take1' sPrev '(Nothing, s) = 'Reply (Err (ETakeEnd 1)) sPrev
