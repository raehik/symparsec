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
    Parser(..), PParser(..), SParser(..)
  , ParserCh, PParserCh, ParserChSym, ParserChSym1, SParserChSym, SParserChSym1
  , ParserEnd, PParserEnd, ParserEndSym, SParserEndSym
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

data Parser str s r = Parser
  { parserCh  :: ParserCh  str s r
  , parserEnd :: ParserEnd str s r
  , parserS0  :: s
  }

-- | A type-level parser, containing defunctionalization symbols.
--
-- Only intended for promoted use. For singled term-level parsers, use
-- 'SParser'. (Symparsec doesn't provide "regular" term-level parsers.)
--
-- I would make this demotable, but the defun symbols get in the way.
data PParser s r = PParser
  { pparserCh  :: ParserChSym s r
  , pparserEnd :: ParserEndSym s r
  , pparserS0  :: s
  }

-- | A singled parser.
--
-- TODO consider swapping for STuple3...? this is much easier though
type SParser
    :: (s -> Type) -> (r -> Type) -> PParser s r
    -> Type
data SParser ss sr p where
    SParser
        :: SParserChSym  ss sr pCh
        -> SParserEndSym ss sr pEnd
        -> ss s0
        -> SParser ss sr ('PParser pCh pEnd s0)

-- | Parse a 'Char' with the given state.
type  ParserCh str s r = Char -> s -> Result str s r
type PParserCh     s r = ParserCh Symbol s r

-- | A defunctionalization symbol for a 'ParserCh'.
type ParserChSym s r = Char ~> s ~> PResult s r

-- | A partially-applied defunctionalization symbol for a 'ParserCh'.
type ParserChSym1 s r = Char -> s ~> PResult s r

type SParserChSym ss sr pCh = Lam2 SChar ss (SResult ss sr) pCh
type SParserChSym1 ch ss sr pCh = SChar ch -> Lam ss (SResult ss sr) (pCh ch)

-- | What a parser should do at the end of a 'Symbol'.
type  ParserEnd str s r = s -> ResultEnd str r
type PParserEnd     s r = ParserEnd Symbol s r

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
--
-- Note that a 'Done' indicates the parser has not consumed the character. In
-- the original design, it did consume it, and parsers did their own "lookahead"
-- to handle this. The non-consuming behaviour simplifies permitting
-- non-consuming parsers such as @Take 0@.
data Result str s r
  = Cont s       -- ^ OK,     consumed, continue with state @s@
  | Done r       -- ^ OK, not consumed, parse succeeded with result @r@
  | Err  (E str) -- ^ parse error

-- | Promoted 'Result'.
type PResult = Result Symbol

data SResult (ss :: s -> Type) (sr :: r -> Type) (res :: PResult s r) where
    SCont :: ss s -> SResult ss sr (Cont s)
    SDone :: sr r -> SResult ss sr (Done r)
    SErr  :: SE e -> SResult ss sr (Err e)

type  ResultEnd str =  Either (E str)
type PResultEnd     =  Either PE
type SResultEnd     = SEither SE

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

-- | Promoted parsers with singled implementations.
class SingParser (p :: PParser s r) where
    -- | A singleton for the parser state.
    type PS  p :: s -> Type

    -- | A singleton for the parser return type.
    type PR  p :: r -> Type

    -- | The singled parser.
    singParser' :: SParser (PS p) (PR p) p

-- | Get the singled version of the requested parser.
--
-- 'singParser'' with better type application ergonomics.
singParser
    :: forall {s} {r} (p :: PParser s r). SingParser p
    => SParser (PS p) (PR p) p
singParser = singParser' @_ @_ @p
