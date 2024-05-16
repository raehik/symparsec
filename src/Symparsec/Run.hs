{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Run where -- ( Run, ERun(..) ) where

import Symparsec.Util ( ReconsSymbol )
import Symparsec.Parser
import GHC.TypeLits ( Symbol, UnconsSymbol )
import GHC.TypeLits qualified as TE
import GHC.TypeNats ( Natural, type (+) )
import DeFun.Core ( type (@@) )
import TypeLevelShow.Doc
import TypeLevelShow.Utils ( ShowChar )
import TypeLevelShow.Natural ( ShowNatDec )

-- | Run the given parser on the given 'Symbol', returning an 'TE.ErrorMessage'
--   on failure.
type Run :: ParserSym s r -> Symbol -> Either TE.ErrorMessage (r, Symbol)
type Run p sym = MapLeftRender (Run' p sym)

type MapLeftRender :: Either PERun r -> Either TE.ErrorMessage r
type family MapLeftRender eer where
    MapLeftRender (Right a) = Right a
    MapLeftRender (Left  e) = Left (RenderDoc (PrettyERun e))

-- | Run the given parser on the given 'Symbol', returning a 'PERun' on failure.
type Run' :: ParserSym s r -> Symbol -> Either PERun (r, Symbol)
type family Run' p sym where
    Run' ('ParserSym pCh pEnd s) sym = RunStart pCh pEnd s (UnconsSymbol sym)

-- | Run the given parser on the given 'Symbol', emitting a type error on
--   failure.
--
-- This /would/ be useful for @:k!@ runs, but it doesn't work properly with
-- 'TE.TypeError's, printing @= (TypeError ...)@ instead of the error message.
-- Alas! Instead, do something like @> Proxy \@(RunTest ...)@.
type RunTest :: ParserSym s r -> Symbol -> (r, Symbol)
type RunTest p sym = MapLeftTypeError (Run p sym)

type MapLeftTypeError :: Either TE.ErrorMessage a -> a
type family MapLeftTypeError eea where
    MapLeftTypeError (Right a) = a
    MapLeftTypeError (Left  e) = TE.TypeError e

type family RunStart pCh pEnd s msym where
    -- | Parsing non-empty string: call main loop
    RunStart pCh pEnd s (Just '(ch, sym)) =
        RunCh pCh pEnd 0 ch (UnconsSymbol sym) (pCh @@ ch @@ s)

    -- | Parsing empty string: call special early exit
    RunStart pCh pEnd s Nothing           = RunEnd0 (pEnd @@ s)

-- | Inspect character parser result.
--
-- This is purposely written so that the main case is at the top, and a single
-- equation (we parse, prepare next character and inspect character parser
-- result at the same time). My hope is that this keeps GHC fast.
type family RunCh pCh pEnd idx ch' msym res where
    -- | OK, and more to come: parse next character
    RunCh pCh pEnd idx ch' (Just '(ch, sym)) (Cont s) =
        RunCh pCh pEnd (idx+1) ch (UnconsSymbol sym) (pCh @@ ch @@ s)

    -- | OK, and we're at the end of the string: run end parser
    RunCh pCh pEnd idx ch' Nothing           (Cont s) =
        RunEnd idx ch' (pEnd @@ s)

    -- | OK, and we're finished early: return value and remaining string
    RunCh pCh pEnd idx ch' msym              (Done r) =
        Right '(r, ReconsSymbol msym)

    -- | Parse error: return error
    RunCh pCh pEnd idx ch' msym              (Err  e) =
        Left ('ERun idx ch' e)

-- | Inspect end parser result.
type RunEnd
    :: Natural -> Char
    -> Either PE r
    -> Either PERun (r, Symbol)
type family RunEnd idx ch res where
    RunEnd idx ch (Right r) = Right '(r, "")
    RunEnd idx ch (Left  e) = Left ('ERun idx ch e)

-- | Inspect end parser result for the empty string, where we have no previous
--   character or (meaningful) index.
type family RunEnd0 res where
    RunEnd0 (Right r) = Right '(r, "")
    RunEnd0 (Left  e) = Left (ERun0 e)

type PrettyERun :: PERun -> PDoc
type family PrettyERun e where
    PrettyERun (ERun0 e) = Text "parse error on empty string" :$$: PrettyE e
    PrettyERun ('ERun idx ch e) =
             Text "parse error at index " :<>: Text (ShowNatDec idx)
        :<>: Text ", char '" :<>: Text (ShowChar ch) :<>: Text "'"
        :$$: PrettyE e

type PrettyE :: PE -> PDoc
type family PrettyE e where
    PrettyE (EBase name emsg)  = Text name :<>: Text ": " :<>: emsg
    PrettyE (EIn   name e)     = Text name :<>: Text ": " :<>: PrettyE e

-- | Error while running parser.
data ERun s
  -- | Parser error at index X, character C.
  = ERun Natural Char (E s)

  -- | Parser error on the empty string.
  | ERun0 (E s)
    deriving stock Show

-- | Promoted 'ERun'.
type PERun = ERun Symbol
