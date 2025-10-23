{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeAbstractions #-}

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
type AsChar :: Char -> PParser s ()
type AsChar c = Literal (ConsSymbol c "")
type AnyChar :: PParser s Char
type AnyChar = Take1
-- TODO: adding the following kind signature makes GHC type error, it
-- initialises an inner s0 and doesn't see s ~ s0. omitting it has GHC
-- understand the kind polymorphism just fine.
-- doesn't happen for other parsers e.g. Literal. why?
-- TODO: looks like it happens with type synonyms. oh dear
--type Number :: PParser s Natural
type Number = NatBaseWhile 10 ParseDigitDecSym

-- special parser that I probably don't want (surely can just combine)
-- backtracks!!
type NotChar :: Char -> PParser s Char
data NotChar c ps
type instance App (NotChar c) ps = NotChar' c ps (UnconsState ps)
type family NotChar' cNo psPrev ps where
    NotChar' cNo psPrev '(Just  c, ps) = If (c == cNo)
        ('Reply (Err (Error1 "got the char we didn't want")) psPrev)
        ('Reply (OK c) ps)
    NotChar' cNo psPrev '(Nothing, ps) = 'Reply (Err (Error1 "empty string")) psPrev

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

type FlagParser :: PParser s Flags
data FlagParser ps
type instance App FlagParser ps = PFlags' EmptyFlags ps (UnconsState ps)

type EmptyFlags = 'Flags Nothing Nothing False

type PFlags' :: Flags -> PState s -> (Maybe Char, PState s) -> PReply s Flags
type family PFlags' flags psPrev mps where
  PFlags' ('Flags d i l) psPrev '(Just '-', ps) =
    PFlags' ('Flags (Just (UpdateAdjust d LeftAdjust)) i l) ps (UnconsState ps)
  PFlags' ('Flags d i l) psPrev '(Just '0', ps) =
    PFlags' ('Flags (Just (UpdateAdjust d ZeroPad))    i l) ps (UnconsState ps)
  PFlags' ('Flags d i l) psPrev '(Just '+', ps) =
    PFlags' ('Flags d (Just (UpdateSign i SignPlus))     l) ps (UnconsState ps)
  PFlags' ('Flags d i l) psPrev '(Just ' ', ps) =
    PFlags' ('Flags d (Just (UpdateSign i SignSpace))    l) ps (UnconsState ps)
  PFlags' ('Flags d i l) psPrev '(Just '#', ps) =
    PFlags' ('Flags d i True)                               ps (UnconsState ps)
  PFlags' flags psPrev '(_, ps) =
    'Reply (OK flags) psPrev

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

type Test1 :: PParser s Char
type Test1 = Take1

type Test2 :: PParser s Char
type Test2 = Test1

type Test3 :: PParser s Char
type Test3 = AnyChar

-- This one works with or without the visible kind argument.
type Test4 :: Char -> PParser s Char
type Test4 c = Pure c

-- Standalone kind signature causes type error.
-- Adding type abstraction syntax fixes.
type Test5 :: PParser s Char
type Test5 @s = Pure @s 'c'

type Test6 :: PParser s a
type Test6 = Empty

-- Kind signature causes type error.
--type Test7 :: PParser s ()
type Test7 = Literal "asd"
