{-# LANGUAGE UndecidableInstances #-}

-- | Common definitions used by parsers.

module Symparsec.Parser.Common
  (
  -- * Common definitions
    type UnconsState
  , type Error1
  --, type Err1
  , type EStrInputTooShort, type EStrWrongChar
  --, type Err, type Done
  , type Impossible

  -- * Re-exports
  , module Symparsec.Parser
  , Doc(..), type (++)
  , type App

  -- ** Common imports
  -- $common-imports
  , type Symbol, type UnconsSymbol, ConsSymbol
  , type Natural, type (+), type (-), type (*)
  , type ShowNatDec, type ShowChar
  , type (@@), type (~>)
  ) where

-- $common-imports
-- TODO idk syntax
-- Not used by all parsers, but common enough that we'll export them here.

import Symparsec.Parser
import DeFun.Core
import GHC.TypeLits ( type Symbol, type UnconsSymbol, type ConsSymbol )
import GHC.TypeNats ( type Natural, type (+), type (-), type (*) )
import TypeLevelShow.Doc
import TypeLevelShow.Natural ( type ShowNatDec )
import TypeLevelShow.Utils ( type ShowChar, type (++) )
-- TODO clean up (++) stuff, probably just export my own one
import GHC.TypeError qualified as TE

-- idk which order is nicer here. @s@ first means "out of the way" for error.
--type Err  s e = 'Reply (Err e) s
--type Done s a = 'Reply (OK  a) s

-- TODO this is pre-spans
--type EBase parserName = 'Error '[(Text parserName) ('Span 0 0) '[]

-- | Get the next character in the string and update the parser state.
--
-- If at end of the string, the state is returned untouched, and @len@ is
-- guaranteed to be 0.
type UnconsState :: PState -> (Maybe Char, PState)
type family UnconsState s where
    UnconsState ('State rem 0   idx) = '(Nothing, 'State rem 0 idx)
    UnconsState ('State rem len idx) = UnconsState' (UnconsSymbol rem) len idx

type UnconsState'
    :: Maybe (Char, Symbol) -> Natural -> Natural -> (Maybe Char, PState)
type family UnconsState' mstr len idx where
    UnconsState' (Just '(ch, rem)) len idx =
        '(Just ch, 'State rem (len-1) (idx+1))
    UnconsState' Nothing           len idx =
        -- TODO could I change this to a regular parser error? should I?
        TE.TypeError (TE.Text "unrecoverable parser error: got to end of input string before len=0")

-- TODO: add type synonym for @(Maybe Char, PState) -> PResult res@

-- TODO
type Error1 str = 'Error '[str]
--type Err1 str = Err (Error1 str)
--type OK' a s = 'Reply (OK a) s

type EStrInputTooShort :: Natural -> Natural -> Symbol
type EStrInputTooShort nNeed nGot =
         "needed " ++ ShowNatDec nNeed
      ++ " chars, but only " ++ ShowNatDec nGot ++ " remain"

type EStrWrongChar :: Char -> Char -> Symbol
type EStrWrongChar chExpect chGot =
         "expected '" ++ ShowChar chExpect
      ++  "', got '"  ++ ShowChar chGot ++ "'"

-- | Impossible parser state.
--
-- Use when you can prove that an equation is impossible.
type Impossible = TE.TypeError (TE.Text "impossible parser state")
