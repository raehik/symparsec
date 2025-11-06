{-# LANGUAGE UndecidableInstances #-}

-- | Running Symparsec parsers.

module Symparsec.Run ( type Run, type Run', type RunTest, type RunTest' ) where

import Symparsec.Parser
import Data.Type.Symbol qualified as Symbol
import DeFun.Core
import GHC.TypeLits ( type Symbol )
import GHC.TypeNats ( type Natural, type (+) )
import GHC.TypeError qualified as TE
import TypeLevelShow.Doc
import TypeLevelShow.Natural ( type ShowNatDec )

-- | Run a parser with some initial custom state on a 'Symbol'.
--
-- * On success, returns a tuple of @(result :: a, remaining :: 'Symbol')@.
-- * On failure, returns an 'TE.ErrorMessage'.
type Run :: PParser s a -> s -> Symbol -> Either TE.ErrorMessage (a, Symbol)
type Run p custom str = RunEnd str (p @@ StateInit custom str)

-- | Run a parser on a 'Symbol'. The parser must not use custom state.
--
-- * On success, returns a tuple of @(result :: a, remaining :: 'Symbol')@.
-- * On failure, returns an 'TE.ErrorMessage'.
type Run' :: PParser () a -> Symbol -> Either TE.ErrorMessage (a, Symbol)
type Run' p str = Run p '() str

type RunEnd :: Symbol -> PReply s a -> Either TE.ErrorMessage (a, Symbol)
type family RunEnd str rep where
    RunEnd str ('Reply (OK  a) ('State _s  rem _len _idx)) =
        -- TODO I could return only @len@ of the remaining input @rem@, but
        -- that's more work than just returning @rem@, and I don't see a way
        -- this would matter for correct parsers.
        Right '(a, rem)
    RunEnd str ('Reply (Err e) ('State _s _rem _len  idx)) =
        Left (RenderPDoc (PrettyErrorTop idx str e))

-- | Run a parser with some initial custom state on a 'Symbol',
--   emitting a type error on failure.
--
-- This /would/ be useful for @:k!@ runs, but it doesn't work properly with
-- 'TE.TypeError's, printing @= (TypeError ...)@ instead of the error message.
-- Alas! Instead, do something like @> Proxy \@(RunTest ...)@.
type RunTest :: PParser s a -> s -> Symbol -> (a, Symbol)
type RunTest p custom str = FromRightTypeError (Run p custom str)

-- | Run a parser on a 'Symbol', emitting a type error on failure.
--   The parser must not use custom. state
--
-- This /would/ be useful for @:k!@ runs, but it doesn't work properly with
-- 'TE.TypeError's, printing @= (TypeError ...)@ instead of the error message.
-- Alas! Instead, do something like @> Proxy \@(RunTest ...)@.
type RunTest' :: PParser () a -> Symbol -> (a, Symbol)
type RunTest' p str = RunTest p '() str

type FromRightTypeError :: Either TE.ErrorMessage a -> a
type family FromRightTypeError eea where
    FromRightTypeError (Right a) = a
    FromRightTypeError (Left  e) = TE.TypeError e

-- | Initial parser state for the given 'Symbol'.
type StateInit :: s -> Symbol -> PState s
type StateInit s str = 'State s str (Symbol.Length str) 0

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
