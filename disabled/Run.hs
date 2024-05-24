{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for reifying/singled parsers

module Symparsec.Run where -- ( Run, ERun(..) ) where

import Symparsec.Parser
import GHC.TypeLits hiding ( ErrorMessage(..), fromSNat )
import GHC.TypeNats ( fromSNat )
import GHC.TypeLits qualified as TE
import DeFun.Core
import TypeLevelShow.Doc
import TypeLevelShow.Utils ( ShowChar )
import TypeLevelShow.Natural ( ShowNatDec )
import Singleraeh.Tuple ( STuple2(..) )
import Singleraeh.Maybe ( SMaybe(..) )
import Singleraeh.Either ( SEither(..) )
import Singleraeh.Symbol ( ReconsSymbol, sReconsSymbol, sUnconsSymbol )
import Singleraeh.Natural ( (%+) )
import Singleraeh.Demote

-- | Run the given parser on the given 'Symbol', returning an 'TE.ErrorMessage'
--   on failure.
type Run :: Parser s r -> Symbol -> Either TE.ErrorMessage (r, Symbol)
type Run p sym = MapLeftRender (Run' p sym)

type MapLeftRender :: Either PERun r -> Either TE.ErrorMessage r
type family MapLeftRender eer where
    MapLeftRender (Right a) = Right a
    MapLeftRender (Left  e) = Left (RenderPDoc (PrettyERun e))

-- | Run the given parser on the given 'Symbol', returning a 'PERun' on failure.
type Run' :: Parser s r -> Symbol -> Either PERun (r, Symbol)
type family Run' p sym where
    Run' ('Parser pCh pEnd s) sym = RunStart pCh pEnd s (UnconsSymbol sym)

-- | Run the singled version of type-level parser on the given 'String',
--   returning an 'ERun' on failure.
--
-- You must provide a function for demoting the singled return type.
-- ('Singleraeh.Demote.demote' can do this for you automatically.)
run'
    :: forall {ps} {pr} (p :: Parser ps pr) r. SingParser p
    => (forall a. PR p a -> r) -> String -> Either (ERun String) (r, String)
run' demotePR str = withSomeSSymbol str $ \sstr ->
    case sRun' (singParser @p) sstr of
      SRight (STuple2 pr sstr') -> Right (demotePR pr, fromSSymbol sstr')
      SLeft  e                  -> Left $ demoteSERun e

sRun'
    :: SParser ss sr p
    -> SSymbol str
    -> SEither SERun (STuple2 sr SSymbol) (Run' p str)
sRun' (SParser pCh pEnd sInit) str =
    sRunStart pCh pEnd sInit (sUnconsSymbol str)

-- | Run the given parser on the given 'Symbol', emitting a type error on
--   failure.
--
-- This /would/ be useful for @:k!@ runs, but it doesn't work properly with
-- 'TE.TypeError's, printing @= (TypeError ...)@ instead of the error message.
-- Alas! Instead, do something like @> Proxy \@(RunTest ...)@.
type RunTest :: Parser s r -> Symbol -> (r, Symbol)
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

sRunStart
    :: SParserChSym  ss sr pCh
    -> SParserEndSym ss sr pEnd
    -> ss s
    -> SMaybe (STuple2 SChar SSymbol) mstr
    -> SEither SERun (STuple2 sr SSymbol) (RunStart pCh pEnd s mstr)
sRunStart pCh pEnd s = \case
  SJust (STuple2 ch str) ->
    sRunCh pCh pEnd (SNat @0) ch (sUnconsSymbol str) (pCh @@ ch @@ s)
  SNothing -> sRunEnd0 (pEnd @@ s)

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

sRunCh
    :: SParserChSym  ss sr pCh
    -> SParserEndSym ss sr pEnd
    -> SNat idx
    -> SChar chPrev
    -> SMaybe (STuple2 SChar SSymbol) mstr
    -> SResult ss sr res
    -> SEither SERun (STuple2 sr SSymbol) (RunCh pCh pEnd idx chPrev mstr res)
sRunCh pCh pEnd idx chPrev mstr = \case
  SCont s ->
    case mstr of
      SJust (STuple2 ch str) ->
        sRunCh pCh pEnd (idx %+ (SNat @1)) ch (sUnconsSymbol str)
            (pCh @@ ch @@ s)
      SNothing ->
        sRunEnd idx chPrev (pEnd @@ s)
  SDone r -> SRight (STuple2 r (sReconsSymbol mstr))
  SErr  e -> SLeft (SERun idx chPrev e)

-- | Inspect end parser result.
type RunEnd
    :: Natural -> Char
    -> Either PE r
    -> Either PERun (r, Symbol)
type family RunEnd idx ch res where
    RunEnd idx ch (Right r) = Right '(r, "")
    RunEnd idx ch (Left  e) = Left ('ERun idx ch e)

sRunEnd
    :: SNat idx -> SChar ch
    -> SEither SE sr res
    -> SEither SERun (STuple2 sr SSymbol) (RunEnd idx ch res)
sRunEnd idx ch = \case
  SRight r -> SRight (STuple2 r (SSymbol @""))
  SLeft  e -> SLeft (SERun idx ch e)

-- | Inspect end parser result for the empty string, where we have no previous
--   character or (meaningful) index.
type family RunEnd0 res where
    RunEnd0 (Right r) = Right '(r, "")
    RunEnd0 (Left  e) = Left (ERun0 e)

sRunEnd0
    :: SEither SE sr res
    -> SEither SERun (STuple2 sr SSymbol) (RunEnd0 res)
sRunEnd0 = \case
  SRight r -> SRight (STuple2 r (SSymbol @""))
  SLeft  e -> SLeft (SERun0 e)

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

data SERun (erun :: PERun) where
    SERun  :: SNat idx -> SChar ch -> SE e -> SERun ('ERun idx ch e)
    SERun0 ::                         SE e -> SERun (ERun0 e)

demoteSERun :: SERun erun -> ERun String
demoteSERun = \case
  SERun  idx ch e -> ERun  (fromSNat idx) (fromSChar ch) (demoteSE e)
  SERun0        e -> ERun0                               (demoteSE e)

instance Demotable SERun where
    type Demote SERun = ERun String
    demote = demoteSERun

---

type XRun' :: Parser' s0 s r -> Symbol -> Either PERun (r, Symbol)
type family XRun' p str where
    XRun' ('Parser' pCh pEnd s0 sInit) str =
        XRunStart pCh pEnd (sInit @@ s0) (UnconsSymbol str)

type family XRunStart pCh pEnd s mstr where
    XRunStart pCh pEnd (Right s) (Just '(ch, str)) =
        RunCh pCh pEnd 0 ch (UnconsSymbol str) (pCh @@ ch @@ s)

    XRunStart pCh pEnd (Right s) Nothing =
        RunEnd0 (pEnd @@ s)

    -- | non-consuming parser on empty string: run end handler
    XRunStart pCh pEnd (Left '(e, s)) Nothing =
        RunEnd0 (pEnd @@ s)

    -- | non-consuming parser on non-empty string: fail
    XRunStart pCh pEnd (Left '(e, s)) (Just cstr) =
        Left (ERun0 e)
