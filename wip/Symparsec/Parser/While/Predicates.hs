-- | Character predicates.

-- TODO for singling, I could cheat by inspecting the char value. should be
-- faster and better. but would need a bit more checking so cba for now.

module Symparsec.Parser.While.Predicates where

import DeFun.Core
import GHC.TypeLits
import Singleraeh.Bool
import Singleraeh.Equality ( testEqElse )
import Unsafe.Coerce ( unsafeCoerce )

class SingChPred chPred where
    singChPred :: Lam SChar SBool chPred

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

sIsAlphaSym :: Lam SChar SBool IsAlphaSym
sIsAlphaSym = Lam $ \ch ->
      testEqElse ch (SChar @'a') STrue
    $ testEqElse ch (SChar @'A') STrue
    $ testEqElse ch (SChar @'b') STrue
    $ testEqElse ch (SChar @'B') STrue
    $ testEqElse ch (SChar @'c') STrue
    $ testEqElse ch (SChar @'C') STrue
    $ testEqElse ch (SChar @'d') STrue
    $ testEqElse ch (SChar @'D') STrue
    $ testEqElse ch (SChar @'e') STrue
    $ testEqElse ch (SChar @'E') STrue
    $ testEqElse ch (SChar @'f') STrue
    $ testEqElse ch (SChar @'F') STrue
    $ testEqElse ch (SChar @'g') STrue
    $ testEqElse ch (SChar @'G') STrue
    $ testEqElse ch (SChar @'h') STrue
    $ testEqElse ch (SChar @'H') STrue
    $ testEqElse ch (SChar @'i') STrue
    $ testEqElse ch (SChar @'I') STrue
    $ testEqElse ch (SChar @'j') STrue
    $ testEqElse ch (SChar @'J') STrue
    $ testEqElse ch (SChar @'k') STrue
    $ testEqElse ch (SChar @'K') STrue
    $ testEqElse ch (SChar @'l') STrue
    $ testEqElse ch (SChar @'L') STrue
    $ testEqElse ch (SChar @'m') STrue
    $ testEqElse ch (SChar @'M') STrue
    $ testEqElse ch (SChar @'n') STrue
    $ testEqElse ch (SChar @'N') STrue
    $ testEqElse ch (SChar @'o') STrue
    $ testEqElse ch (SChar @'O') STrue
    $ testEqElse ch (SChar @'p') STrue
    $ testEqElse ch (SChar @'P') STrue
    $ testEqElse ch (SChar @'q') STrue
    $ testEqElse ch (SChar @'Q') STrue
    $ testEqElse ch (SChar @'r') STrue
    $ testEqElse ch (SChar @'R') STrue
    $ testEqElse ch (SChar @'s') STrue
    $ testEqElse ch (SChar @'S') STrue
    $ testEqElse ch (SChar @'t') STrue
    $ testEqElse ch (SChar @'T') STrue
    $ testEqElse ch (SChar @'u') STrue
    $ testEqElse ch (SChar @'U') STrue
    $ testEqElse ch (SChar @'v') STrue
    $ testEqElse ch (SChar @'V') STrue
    $ testEqElse ch (SChar @'w') STrue
    $ testEqElse ch (SChar @'W') STrue
    $ testEqElse ch (SChar @'x') STrue
    $ testEqElse ch (SChar @'X') STrue
    $ testEqElse ch (SChar @'y') STrue
    $ testEqElse ch (SChar @'Y') STrue
    $ testEqElse ch (SChar @'z') STrue
    $ testEqElse ch (SChar @'Z') STrue
    $ unsafeCoerce SFalse

instance SingChPred IsAlphaSym where singChPred = sIsAlphaSym

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

sIsHexDigitSym :: Lam SChar SBool IsHexDigitSym
sIsHexDigitSym = Lam $ \ch ->
      testEqElse ch (SChar @'0') STrue
    $ testEqElse ch (SChar @'1') STrue
    $ testEqElse ch (SChar @'2') STrue
    $ testEqElse ch (SChar @'3') STrue
    $ testEqElse ch (SChar @'4') STrue
    $ testEqElse ch (SChar @'5') STrue
    $ testEqElse ch (SChar @'6') STrue
    $ testEqElse ch (SChar @'7') STrue
    $ testEqElse ch (SChar @'8') STrue
    $ testEqElse ch (SChar @'9') STrue
    $ testEqElse ch (SChar @'a') STrue
    $ testEqElse ch (SChar @'A') STrue
    $ testEqElse ch (SChar @'b') STrue
    $ testEqElse ch (SChar @'B') STrue
    $ testEqElse ch (SChar @'c') STrue
    $ testEqElse ch (SChar @'C') STrue
    $ testEqElse ch (SChar @'d') STrue
    $ testEqElse ch (SChar @'D') STrue
    $ testEqElse ch (SChar @'e') STrue
    $ testEqElse ch (SChar @'E') STrue
    $ testEqElse ch (SChar @'f') STrue
    $ testEqElse ch (SChar @'F') STrue
    $ unsafeCoerce SFalse

instance SingChPred IsHexDigitSym where singChPred = sIsHexDigitSym
