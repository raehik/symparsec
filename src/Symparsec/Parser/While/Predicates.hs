-- | Character predicates.
--
-- raehik copied his module from Symparsec.

module Symparsec.Parser.While.Predicates where

import DeFun.Core

-- | @A-Za-z@
type IsAlpha :: Char -> Bool
type family IsAlpha ch where
    IsAlpha 'a' = True
    IsAlpha 'A' = True
    IsAlpha 'b' = True
    IsAlpha 'B' = True
    IsAlpha 'c' = True
    IsAlpha 'C' = True
    IsAlpha 'd' = True
    IsAlpha 'D' = True
    IsAlpha 'e' = True
    IsAlpha 'E' = True
    IsAlpha 'f' = True
    IsAlpha 'F' = True
    IsAlpha 'g' = True
    IsAlpha 'G' = True
    IsAlpha 'h' = True
    IsAlpha 'H' = True
    IsAlpha 'i' = True
    IsAlpha 'I' = True
    IsAlpha 'j' = True
    IsAlpha 'J' = True
    IsAlpha 'k' = True
    IsAlpha 'K' = True
    IsAlpha 'l' = True
    IsAlpha 'L' = True
    IsAlpha 'm' = True
    IsAlpha 'M' = True
    IsAlpha 'n' = True
    IsAlpha 'N' = True
    IsAlpha 'o' = True
    IsAlpha 'O' = True
    IsAlpha 'p' = True
    IsAlpha 'P' = True
    IsAlpha 'q' = True
    IsAlpha 'Q' = True
    IsAlpha 'r' = True
    IsAlpha 'R' = True
    IsAlpha 's' = True
    IsAlpha 'S' = True
    IsAlpha 't' = True
    IsAlpha 'T' = True
    IsAlpha 'u' = True
    IsAlpha 'U' = True
    IsAlpha 'v' = True
    IsAlpha 'V' = True
    IsAlpha 'w' = True
    IsAlpha 'W' = True
    IsAlpha 'x' = True
    IsAlpha 'X' = True
    IsAlpha 'y' = True
    IsAlpha 'Y' = True
    IsAlpha 'z' = True
    IsAlpha 'Z' = True
    IsAlpha _   = False

type IsAlphaSym :: Char ~> Bool
data IsAlphaSym ch
type instance App IsAlphaSym ch = IsAlpha ch

-- | @0-9A-Fa-f@
type IsHexDigit :: Char -> Bool
type family IsHexDigit ch where
    IsHexDigit '0' = True
    IsHexDigit '1' = True
    IsHexDigit '2' = True
    IsHexDigit '3' = True
    IsHexDigit '4' = True
    IsHexDigit '5' = True
    IsHexDigit '6' = True
    IsHexDigit '7' = True
    IsHexDigit '8' = True
    IsHexDigit '9' = True
    IsHexDigit 'a' = True
    IsHexDigit 'A' = True
    IsHexDigit 'b' = True
    IsHexDigit 'B' = True
    IsHexDigit 'c' = True
    IsHexDigit 'C' = True
    IsHexDigit 'd' = True
    IsHexDigit 'D' = True
    IsHexDigit 'e' = True
    IsHexDigit 'E' = True
    IsHexDigit 'f' = True
    IsHexDigit 'F' = True
    IsHexDigit _   = False

type IsHexDigitSym :: Char ~> Bool
data IsHexDigitSym ch
type instance App IsHexDigitSym ch = IsHexDigit ch

-- | @0-9@
type IsDecDigit :: Char -> Bool
type family IsDecDigit ch where
    IsDecDigit '0' = True
    IsDecDigit '1' = True
    IsDecDigit '2' = True
    IsDecDigit '3' = True
    IsDecDigit '4' = True
    IsDecDigit '5' = True
    IsDecDigit '6' = True
    IsDecDigit '7' = True
    IsDecDigit '8' = True
    IsDecDigit '9' = True
    IsDecDigit _   = False

type IsDecDigitSym :: Char ~> Bool
data IsDecDigitSym ch
type instance App IsDecDigitSym ch = IsDecDigit ch

type Elem :: [a] -> a -> Bool 
type family Elem where 
  Elem (_:xs) x = Elem xs x
  Elem '[] _ = 'False
  Elem (x':_) x = 'True
data ElemSym lst ch
type instance (App (ElemSym lst)) a = Elem lst a
