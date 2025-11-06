{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Take ( type Take, type TakeSym, type Take1 ) where

import Symparsec.Parser.Common
import Singleraeh.Symbol ( type RevCharsToSymbol )

-- | Return the next @n@ characters.
type Take :: Natural -> PParser s Symbol
data Take n ps
type instance App (Take n) ps = Take' '[] n ps (UnconsState ps)
type family Take' chs n psPrev ps where
    Take' chs 0 psPrev _             = 'Reply (OK (RevCharsToSymbol chs)) psPrev
    Take' chs n psPrev '(Just ch, ps) = Take' (ch:chs) (n-1) ps (UnconsState ps)
    Take' chs n psPrev '(Nothing, ps) = 'Reply (Err (ETakeEnd n)) psPrev

type ETakeEnd :: Natural -> PError
type ETakeEnd n = Error1
    ( "tried to take " ++ ShowNatDec n ++ " chars from empty string" )

-- | 'Take' defunctionalization symbol.
type TakeSym :: Natural ~> PParser s Symbol
data TakeSym n
type instance App TakeSym n = Take n

-- | Return the next character.
type Take1 :: PParser s Char
data Take1 ps
type instance App Take1 ps = Take1' ps (UnconsState ps)
type family Take1' psPrev ps where
    Take1' psPrev '(Just ch, ps) = 'Reply (OK ch) ps
    Take1' psPrev '(Nothing, ps) = 'Reply (Err (ETakeEnd 1)) psPrev
