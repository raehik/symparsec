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
-- * On success, returns a tuple of @(result :: a, remaining :: 'Symbol')@.
-- * On failure, returns an 'TE.ErrorMessage'.
type Run :: PParserSym a -> Symbol -> Either TE.ErrorMessage (a, Symbol)
type Run p str = RunEnd str (p @@ ('State str (Symbol.Length str) 0))

type family RunEnd str rep where
    RunEnd str ('Reply (OK  a) ('State  rem _len _idx)) =
        -- TODO I could return only @len@ of the remaining input @rem@, but
        -- that's more work than just returning @rem@, and I don't see a way
        -- this would matter for correct parsers.
        Right '(a, rem)
    RunEnd str ('Reply (Err e) ('State _rem _len  idx)) =
        Left (RenderPDoc (PrettyErrorTop idx str e))

-- | Pretty print a top-level parser error.
--
-- Tries to look a bit like Megaparsec and modern compiler parser errors,
-- except we don't actually track much for now (e.g. no line numbers, spans).
type PrettyErrorTop :: Natural -> Symbol -> PError -> PDoc
type PrettyErrorTop idx str e =
             -- idx+1 because we're emitting char position here, not index
             Text "Symparsec parse error:"
        :$$: Text "1:" :<>: Text (ShowNatDec (idx+1))
        :$$: PrettyErrorPosition idx str
        :$$: PrettyError e

-- | Print a 'Symbol' and some ASCII art highlighting a character index in it.
--
-- Looks like Megaparsec and other modern compiler parser errors.
type PrettyErrorPosition :: Natural -> Symbol -> PDoc
type PrettyErrorPosition idx str =
             Text "  |"
        :$$: Text "1 | " :<>: Text str
        :$$: Text "  | " :<>: Text (Symbol.Replicate idx ' ') :<>: Text "^"

-- | Pretty print a parser error.
type PrettyError :: PError -> PDoc
type family PrettyError e where
    PrettyError ('Error (str:strs)) = ConcatSymbol (Text str) strs
    PrettyError ('Error '[])        = Text "<no detail>"

type ConcatSymbol :: PDoc -> [Symbol] -> PDoc
type family ConcatSymbol doc strs where
    -- TODO get ordering right
    ConcatSymbol doc (str:strs) = ConcatSymbol (doc :$$: Text str) strs
    ConcatSymbol doc '[]        = doc

-- | TODO
type RunTest :: PParserSym r -> Symbol -> (r, Symbol)
type RunTest p str = RunTestEnd (Run p str)

type RunTestEnd :: Either TE.ErrorMessage (r, Symbol) -> (r, Symbol)
type family RunTestEnd res where
    RunTestEnd (Right a) = a
    RunTestEnd (Left  e) = TE.TypeError e
