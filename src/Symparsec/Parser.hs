{-# LANGUAGE AllowAmbiguousTypes #-} -- for singling

{- | Base definitions for Symparsec parsers.

Some types are useable both on term-level, and promoted on type-level e.g.
'Result'. For ease of use, you can access the promoted version via type synonyms
like 'PResult' (for promoted X). (This pattern is copied from @singletons@.)

Some definitions I use:

  * "defun symbol": Short for "defunctionalization symbol". A method of passing
    type-level functions (type families) around, without applying them. We use
    phadej's @defun@ library for the basic functionality.
  * "consuming [parser]": A parser which must consume its input. Symparsec
    parsers are always consuming, as it helps keep parser running simple. This
    is problematic, as you can define non-consuming parsers e.g. @Take 0@. We
    handle this by preprocessing initial parser state, to check for such cases.
-}

module Symparsec.Parser
  (
  -- * Parser
    Parser(..), SParser(..)
  , ParserCh, ParserChSym, ParserChSym1, SParserChSym, SParserChSym1
  , ParserEnd, ParserEndSym, SParserEndSym
  , ParserSInit, ParserSInitSym, SParserSInitSym
  , Result(..), PResult, SResult(..)
  , PResultEnd, SResultEnd
  , PResultSInit, SResultSInit

  -- * Error
  , E(..), PE, SE(..), demoteSE, SingE(singE), withSingE

  -- * Singling
  , SingParser(..)
  , singParser
  ) where

import GHC.TypeLits
import DeFun.Core
import GHC.Exts ( withDict )
import TypeLevelShow.Doc
import Singleraeh.Either ( SEither )
import Singleraeh.Demote
import Singleraeh.Tuple ( STuple2 )
import Data.Kind ( Type )

-- | A type-level parser, containing defunctionalization symbols.
--
-- Only intended for promoted use. For singled term-level parsers, use
-- 'SParser'. (Symparsec doesn't provide "regular" term-level parsers.)
--
-- I would make this demotable, but the defun symbols get in the way.
data Parser s0 s r = Parser
  { parserCh    :: ParserChSym s r
  -- ^ A consuming 'Char' parser (defun symbol).

  , parserEnd   :: ParserEndSym s r
  -- ^ An end handler (defun symbol).

  , parserS0    :: s0
  -- ^ Initial raw parser state.
  --
  -- Do not apply type families here. Instead, do so in 'parserSInit'.
  -- That way, we can do those checks in singled parsers too.

  , parserSInit :: ParserSInitSym s0 s
  -- ^ Parser state initializer (defun symbol)
  --
  -- The runner passes the initial raw state at 'parserS0' to this function.
  -- We defunctionalize state initialization in this way to better support
  -- singling parsers.
  }

-- TODO provide a Parser' type synonym and helpers where s0 ~ s, and sInit is
-- just Right

-- | A singled parser.
--
-- TODO consider swapping for STuple3...? this is much easier though
type SParser
    :: (s0 -> Type) -> (s -> Type) -> (r -> Type) -> Parser s0 s r
    -> Type
data SParser ss0 ss sr p where
    SParser
        :: SParserChSym ss sr pCh
        -> SParserEndSym ss sr pEnd
        -> ss0 s0
        -> SParserSInitSym ss0 ss sInit
        -> SParser ss0 ss sr ('Parser pCh pEnd s0 sInit)

-- | Parse a 'Char' with the given state.
--
-- The action is always consuming. For this reason, you may need to look ahead
-- for the final case, so as to not consume an extra 'Char'. This prevents many
-- zero-length parsers. It's a bit weird. See
-- 'Data.Type.Symbol.Parser.Parser.Drop' for an example.
type ParserCh s r = Char -> s -> PResult s r

-- | A defunctionalization symbol for a 'ParserCh'.
type ParserChSym s r = Char ~> s ~> PResult s r

-- | A partially-applied defunctionalization symbol for a 'ParserCh'.
type ParserChSym1 s r = Char -> s ~> PResult s r

type SParserChSym ss sr pCh = Lam2 SChar ss (SResult ss sr) pCh
type SParserChSym1 ch ss sr pCh = SChar ch -> Lam ss (SResult ss sr) (pCh ch)

-- | What a parser should do at the end of a 'Symbol'.
type ParserEnd s r = s -> PResultEnd r

-- | A defunctionalization symbol for a 'ParserEnd'.
type ParserEndSym s r = s ~> PResultEnd r

type SParserEndSym ss sr pEnd = Lam ss (SResultEnd sr) pEnd

type ParserSInit s0 s = s0 -> PResultSInit s
type ParserSInitSym s0 s = s0 ~> PResultSInit s
type SParserSInitSym ss0 ss sInit = Lam ss0 (SResultSInit ss) sInit

-- | The result of a single step of a parser.
--
-- Promotable. Instantiate with 'String' for term-level, 'Symbol' for
-- type-level.
data Result str s r
  = Cont s       -- ^ OK, continue with the given state
  | Done r       -- ^ OK, parse successful with result @r@
  | Err  (E str) -- ^ parse error

-- | Promoted 'Result'.
type PResult = Result Symbol

data SResult (ss :: s -> Type) (sr :: r -> Type) (res :: PResult s r) where
    SCont :: ss s -> SResult ss sr (Cont s)
    SDone :: sr r -> SResult ss sr (Done r)
    SErr  :: SE e -> SResult ss sr (Err e)

type PResultEnd = Either PE
type SResultEnd = SEither SE

type PResultSInit s = Either (PE, s) s
type SResultSInit ss = SEither (STuple2 SE ss) ss

-- | Parser error.
--
-- Promotable. Instantiate with 'String' for term-level, 'Symbol' for
-- type-level.
data E str
  -- | Base parser error.
  = EBase
        str       -- ^ parser name
        (Doc str) -- ^ error message

  -- | Inner parser error inside combinator.
  | EIn
        str     -- ^ combinator name
        (E str) -- ^ inner error
    deriving stock Show

-- | Promoted 'E'.
type PE = E Symbol

data SE (e :: PE) where
    SEBase :: SSymbol name -> SDoc doc -> SE (EBase name doc)
    SEIn   :: SSymbol name -> SE   e   -> SE (EIn name e)

demoteSE :: SE e -> E String
demoteSE = \case
  SEBase sname sdoc -> EBase (fromSSymbol sname) (demoteDoc sdoc)
  SEIn   sname se   -> EIn   (fromSSymbol sname) (demoteSE  se)

instance Demotable SE where
    type Demote SE = E String
    demote = demoteSE

class SingE (e :: PE) where singE :: SE e
instance (KnownSymbol name, SingDoc doc) => SingE (EBase name doc) where
    singE = SEBase (SSymbol @name) (singDoc @doc)
instance (KnownSymbol name, SingE e) => SingE (EIn name e) where
    singE = SEIn   (SSymbol @name) (singE   @e)

withSingE :: forall e r. SE e -> (SingE e => r) -> r
withSingE = withDict @(SingE e)

-- | Parsers with singled implementations.
class SingParser (p :: Parser s0 s r) where
    -- | A singleton for the parser's raw initial state.
    type PS0 p :: s0 -> Type

    -- | A singleton for the parser state.
    type PS  p :: s -> Type

    -- | A singleton for the parser return type.
    type PR  p :: r -> Type

    -- | The singled parser.
    singParser' :: SParser (PS0 p) (PS p) (PR p) p

-- | Get the singled version of the requested parser.
--
-- 'singParser'' with better type application ergonomics.
singParser
    :: forall {s0} {s} {r} (p :: Parser s0 s r). SingParser p
    => SParser (PS0 p) (PS p) (PR p) p
singParser = singParser' @_ @_ @_ @p
