module Symparsec.Example.TaiwanId where

{-

-- Note that the actual library only validates the 'Symbol' on the type level,
-- then reparses using the term-level parser.

data TaiwanId digit digit1289 letter = TaiwanId
  { c0 :: letter
  , c1 :: digit1289
  , c2 :: digit
  , c3 :: digit
  , c4 :: digit
  , c5 :: digit
  , c6 :: digit
  , c7 :: digit
  , c8 :: digit
  }

-- | Represents a single decimal digit from the set {@1@, @2@, @8@, @9@}.
--
data Digit1289
  = D1289_1 | D1289_2 | D1289_8 | D1289_9
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass Finitary

-- | Represents a single decimal digit in the range @0@ to @9@.
--
data Digit
  = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving stock (Bounded, Enum, Eq, Generic, Ord)
  deriving anyclass Finitary

data Letter
  = A | B | C | D | E | F | G | H | I | J | K | L | M
  | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass Finitary

type PTaiwanId :: -> PParser u 
type family PTaiwanId where

type PDigit1289 = OneOf ['1', '2', '8', '9']

-}
