{- | Parse digits from type-level 'Char's.

A 'Nothing' indicates the given 'Char' was not a valid digit for the given base.
-}
module Data.Type.Char.Digits where

import GHC.TypeLits

-- | Parse a binary digit (0 or 1).
type family ParseBinaryDigit (ch :: Char) :: Maybe Natural where
    ParseBinaryDigit '0' = 'Just 0
    ParseBinaryDigit '1' = 'Just 1
    ParseBinaryDigit _   = 'Nothing

-- | Parse an octal digit (0-7).
type family ParseOctalDigit (ch :: Char) :: Maybe Natural where
    ParseOctalDigit '0' = 'Just 0
    ParseOctalDigit '1' = 'Just 1
    ParseOctalDigit '2' = 'Just 2
    ParseOctalDigit '3' = 'Just 3
    ParseOctalDigit '4' = 'Just 4
    ParseOctalDigit '5' = 'Just 5
    ParseOctalDigit '6' = 'Just 6
    ParseOctalDigit '7' = 'Just 7
    ParseOctalDigit _   = 'Nothing

-- | Parse a decimal digit (0-9).
type family ParseDecimalDigit (ch :: Char) :: Maybe Natural where
    ParseDecimalDigit '0' = 'Just 0
    ParseDecimalDigit '1' = 'Just 1
    ParseDecimalDigit '2' = 'Just 2
    ParseDecimalDigit '3' = 'Just 3
    ParseDecimalDigit '4' = 'Just 4
    ParseDecimalDigit '5' = 'Just 5
    ParseDecimalDigit '6' = 'Just 6
    ParseDecimalDigit '7' = 'Just 7
    ParseDecimalDigit '8' = 'Just 8
    ParseDecimalDigit '9' = 'Just 9
    ParseDecimalDigit _   = 'Nothing

-- | Parse a hexadecimal digit (0-9A-Fa-f).
--
-- Both upper and lower case are permitted.
type family ParseHexDigit (ch :: Char) :: Maybe Natural where
    ParseHexDigit '0' = 'Just 0
    ParseHexDigit '1' = 'Just 1
    ParseHexDigit '2' = 'Just 2
    ParseHexDigit '3' = 'Just 3
    ParseHexDigit '4' = 'Just 4
    ParseHexDigit '5' = 'Just 5
    ParseHexDigit '6' = 'Just 6
    ParseHexDigit '7' = 'Just 7
    ParseHexDigit '8' = 'Just 8
    ParseHexDigit '9' = 'Just 9
    ParseHexDigit 'a' = 'Just 10
    ParseHexDigit 'A' = 'Just 10
    ParseHexDigit 'b' = 'Just 11
    ParseHexDigit 'B' = 'Just 11
    ParseHexDigit 'c' = 'Just 12
    ParseHexDigit 'C' = 'Just 12
    ParseHexDigit 'd' = 'Just 13
    ParseHexDigit 'D' = 'Just 13
    ParseHexDigit 'e' = 'Just 14
    ParseHexDigit 'E' = 'Just 14
    ParseHexDigit 'f' = 'Just 15
    ParseHexDigit 'F' = 'Just 15
    ParseHexDigit _   = 'Nothing
