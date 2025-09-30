{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Common
  (
  -- * Re-exports
    module Symparsec2.Parser
  , Doc(..)
  , type App

  -- * Common definitions
  , type UnconsState
  , type Err, type Done
  ) where

import Symparsec2.Parser
import DeFun.Core
import GHC.TypeLits ( type Symbol, type UnconsSymbol )
import GHC.TypeNats ( type Natural, type (+), type (-) )
import TypeLevelShow.Doc
import GHC.TypeError qualified as TE

-- idk which order is nicer here. @s@ first means "out of the way" for error.
type Err  s e = 'Result (Left  e) s
type Done s r = 'Result (Right r) s

-- | Get the next character in the string and update the parser bookkeeping.
--
-- If at end of the string, the index is _not_ updated, and @len@ is always 0.
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
