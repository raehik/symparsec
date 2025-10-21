{-# LANGUAGE UndecidableInstances #-}

{- | The typelits-printf format string parser.

Quick and messy attempt. I purposely choose to stick closely to the original
design, including making a kind of "compatibility layer" that maps some of the
typelits-printf combinators to Symparsec.
-}

module Symparsec.Example.Printf where

import Symparsec.Parser.Common
import Symparsec.Parser.Functor
import Symparsec.Parser.Applicative
import Symparsec.Parser.Alternative
import Symparsec.Parser.Literal
import Symparsec.Parser.Natural
import Symparsec.Parser.Natural.Digits
import Symparsec.Parser.Take
import Text.Printf (FormatAdjustment (..), FormatSign (..))
import DeFun.Core
import Data.Type.Equality ( type (==) )
import Data.Type.Bool ( type If )
import Symparsec.Parser.Try

-- typelits-printf compatibility layer
-- typelits-printf actually does @<* Pure c@ after. but I prefer not to
type AsChar c = Literal (ConsSymbol c "")
type AnyChar = Take1
type Number = NatBaseWhile 10 ParseDigitDecSym

-- special parser that I probably don't want (surely can just combine)
-- backtracks!!
type NotChar :: Char -> PParser Char
data NotChar c s
type instance App (NotChar c) s = NotChar' c s (UnconsState s)
type family NotChar' cNo sPrev s where
    NotChar' cNo sPrev '(Just  c, s) = If (c == cNo)
        ('Reply (Err (Error1 "got the char we didn't want")) sPrev)
        ('Reply (OK c) s)
    NotChar' cNo sPrev '(Nothing, s) = 'Reply (Err (Error1 "empty string")) sPrev

-- extras missing from defun-core
type Con4 :: (a -> b -> c -> d -> e) -> a ~> b ~> c ~> d ~> e
data Con4 con arg
type instance App (Con4 f) arg = Con3 (f arg)
type Con5 :: (a -> b -> c -> d -> e -> f) -> a ~> b ~> c ~> d ~> e ~> f
data Con5 con arg
type instance App (Con5 f) arg = Con4 (f arg)

---

data WidthMod
  = WMhh
  | WMh
  | WMl
  | WMll
  | WML

-- copied as-is
type WMParser =
  (AsChar 'h' *> ((WMhh <$ AsChar 'h') <|> Pure WMh))
    <|> (AsChar 'l' *> ((WMll <$ AsChar 'l') <|> Pure WMl))
    <|> (WML <$ AsChar 'L')

---

data Flags = Flags
  { fAdjust :: Maybe FormatAdjustment
  , fSign :: Maybe FormatSign
  , fAlternate :: Bool
  }

type FlagParser :: PParser Flags
data FlagParser s
type instance App FlagParser s = PFlags' EmptyFlags s (UnconsState s)

type EmptyFlags = 'Flags Nothing Nothing False

type PFlags' :: Flags -> PState -> (Maybe Char, PState) -> PReply Flags
type family PFlags' flags sPrev s where
  PFlags' ('Flags d i l) sPrev '(Just '-', s) =
    PFlags' ('Flags (Just (UpdateAdjust d LeftAdjust)) i l) s (UnconsState s)
  PFlags' ('Flags d i l) sPrev '(Just '0', s) =
    PFlags' ('Flags (Just (UpdateAdjust d ZeroPad))    i l) s (UnconsState s)
  PFlags' ('Flags d i l) sPrev '(Just '+', s) =
    PFlags' ('Flags d (Just (UpdateSign i SignPlus))     l) s (UnconsState s)
  PFlags' ('Flags d i l) sPrev '(Just ' ', s) =
    PFlags' ('Flags d (Just (UpdateSign i SignSpace))    l) s (UnconsState s)
  PFlags' ('Flags d i l) sPrev '(Just '#', s) =
    PFlags' ('Flags d i True)                               s (UnconsState s)
  PFlags' flags sPrev '(_, s) =
    'Reply (OK flags) sPrev

type family UpdateAdjust d1 d2 where
  UpdateAdjust Nothing d2 = d2
  UpdateAdjust (Just LeftAdjust) d2 = LeftAdjust
  UpdateAdjust (Just ZeroPad) d2 = d2

type family UpdateSign i1 i2 where
  UpdateSign Nothing i2 = i2
  UpdateSign (Just SignPlus) i2 = SignPlus
  UpdateSign (Just SignSpace) i2 = i2

---

data FieldFormat = FF
  { fmtFlags :: Flags
  , fmtWidth :: Maybe Natural
  , fmtPrecision :: Maybe Natural
  , fmtWidthMod :: Maybe WidthMod
  , fmtChar :: Char
  }

-- copied as-is except for 'Con5'
type FFParser :: PParser FieldFormat
type FFParser = Con5 FF
    <$> FlagParser
    <*> Optional Number
    <*> Optional (AsChar '.' *> Number)
    <*> Optional WMParser
    <*> AnyChar

---

-- tweaked:
-- * added Pure because my 'AsChar' parser is different (imo better)
-- * had to add Try in a specific location, left-right
-- * removed Cat (could probably add back)
type FmtStrParser =
  Many
    ( (Con1 Left <$> ((Some (NotChar '%' <|> Try (AsChar '%' *> AsChar '%' *> Pure '%')))))
        <|> (Con1 Right <$> (AsChar '%' *> FFParser))
    )
