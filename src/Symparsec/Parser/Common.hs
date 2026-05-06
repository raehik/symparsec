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

import Symparsec.Parser
import DeFun.Core
import GHC.TypeLits ( type Symbol, type UnconsSymbol, type ConsSymbol )
import GHC.TypeNats ( type Natural, type (+), type (-), type (*) )
import TypeLevelShow.Doc
import TypeLevelShow.Natural ( type ShowNatDec )
import TypeLevelShow.Utils ( type ShowChar, type (++) )
-- TODO clean up (++) stuff, probably just export my own one
import GHC.TypeError qualified as TE

-- $common-imports
-- Not used by all parsers, but common enough that we'll export them here.

-- TODO this is pre-spans
--type EBase parserName = 'Error '[(Text parserName) ('Span 0 0) '[]

-- | Get the next character in the string and update the parser state.
--
-- If at end of the string, the state is returned untouched, and @len@ is
-- guaranteed to be 0.
type UnconsState :: PState u -> (Maybe Char, PState u)
type family UnconsState s where
    UnconsState ('State rem 0   idx user) = '(Nothing, 'State rem 0 idx user)
    UnconsState ('State rem len idx user) = UnconsState' (UnconsSymbol rem) len idx user

type UnconsState'
    :: Maybe (Char, Symbol) -> Natural -> Natural -> u -> (Maybe Char, PState u)
type family UnconsState' mstr len idx user where
    UnconsState' (Just '(ch, rem)) len idx user =
        '(Just ch, 'State rem (len-1) (idx+1) user)
    UnconsState' Nothing           len idx user =
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
