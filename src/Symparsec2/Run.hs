{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Run where

import Symparsec2.Parser
import Data.Type.Symbol qualified as Symbol
import DeFun.Core
import GHC.TypeLits ( type Symbol )
import GHC.TypeNats ( type Natural, type (+) )
import GHC.TypeError qualified as TE
import TypeLevelShow.Doc
import TypeLevelShow.Natural ( type ShowNatDec )

-- | Run the given parser on the given 'Symbol'.
--
-- * On success, returns a tuple of @(result :: r, remaining :: 'Symbol')@.
-- * On failure, returns an 'TE.ErrorMessage'.
type Run :: PParserSym r -> Symbol -> Either TE.ErrorMessage (r, Symbol)
type Run p str = RunEnd str (p @@ ('State str (Symbol.Length str) 0))

type family RunEnd str res where
    RunEnd str ('Result (Right r) ('State  rem _len _idx)) =
        -- TODO I could return only @len@ of the remaining input @rem@, but
        -- that's more work than just returning @rem@, and I don't see a way
        -- this would matter for correct parsers.
        Right '(r, rem)
    RunEnd str ('Result (Left  e) ('State _rem _len  idx)) =
        Left (RenderPDoc (PrettyERun idx str e))

-- | Pretty print a top-level parser error.
--
-- Tries to look a bit like Megaparsec and modern compiler parser errors,
-- except we don't track line numbers or error extents (for now).
type PrettyERun :: Natural -> Symbol -> PE -> PDoc
type PrettyERun idx str e =
             -- idx+1 because we're emitting char position here, not index
             Text "1:" :<>: Text (ShowNatDec (idx+1))
        :$$: PrettyErrorPosition idx str
        :$$: PrettyE e

-- | Print a 'Symbol' and some ASCII art highlighting a character index in it.
--
-- Tries to look a bit like Megaparsec and modern compiler parser errors,
-- except we don't track line numbers or error extents (for now).
type PrettyErrorPosition :: Natural -> Symbol -> PDoc
type PrettyErrorPosition idx str =
             Text "  |"
        :$$: Text "1 | " :<>: Text str
        :$$: Text "  | " :<>: Text (Symbol.Replicate idx ' ') :<>: Text "^"

-- | Pretty print a parser error.
--
-- TODO, lazy.
type PrettyE :: PE -> PDoc
type family PrettyE e where
    PrettyE (EBase name emsg) = Text name :<>: Text ": " :<>: emsg
    PrettyE (EIn   name e)    = Text name :<>: Text ": " :<>: PrettyE e

-- | TODO
type RunTest :: PParserSym r -> Symbol -> (r, Symbol)
type RunTest p str = RunTestEnd (Run p str)

type RunTestEnd :: Either TE.ErrorMessage (r, Symbol) -> (r, Symbol)
type family RunTestEnd res where
    RunTestEnd (Right a) = a
    RunTestEnd (Left  e) = TE.TypeError e
