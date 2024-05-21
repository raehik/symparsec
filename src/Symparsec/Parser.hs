{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Base definitions for Symparsec parsers.

Types are designed to be useable on both term and type level where possible.
Type synonyms are provided for promoted types, for saving keystrokes.

Some notes when writing parsers:

  * No need to insert the previous character into error messages, the runner
    does that.
-}

module Symparsec.Parser where
{-
  (
  -- * Parsers
    Parser(..)
  , ParserCh
  , ParserEnd
  , Result(..)
  , E(..)
  , ReifyE(reifyE)

  -- * Defun symbols
  , ParserSym(..)
  , ParserChSym
  , ParserChSym1
  , ParserEndSym

  -- * Promoted types
  , PParserCh
  , PParserEnd
  , PResult
  , PE
  ) where
-}

import GHC.TypeLits
import DeFun.Core ( type (~>), Lam, Lam2 )
import GHC.Exts ( proxy#, withDict )
import TypeLevelShow.Doc
import Singleraeh.Either ( SEither )
import Data.Kind ( Type )

-- | A type-level parser, containing defunctionalization symbols.
data Parser s r = Parser
  { parserCh    :: ParserChSym s r
  -- ^ A defunctionalization symbol for a character parser.
  , parserEnd   :: ParserEndSym s r
  -- ^ A defunctionalization symbol for an end handler.
  , parserSInit :: s
  -- ^ Initial parser state.
  }

{-
-- | A term-level parser.
--
-- Parsers contain a character parser, an end handler, and an initial state.
--
-- May not be promoted due to containing function types.
data Parser str s r = Parser
  { parserCh   :: ParserCh  str s r
  -- ^ Character parser.
  , parserEnd  :: ParserEnd str s r
  -- ^ End handler.
  , parserInit :: s
  -- ^ Initial parser state.
  }
-}

class SingParser (p :: Parser s r) where
    type PS p :: s -> Type
    type PR p :: r -> Type
    singParser' :: SParser (PS p) (PR p) p

singParser
    :: forall {s} {r} (p :: Parser s r). SingParser p
    => SParser (PS p) (PR p) p
singParser = singParser' @_ @_ @p

type SParser :: (s -> Type) -> (r -> Type) -> Parser s r -> Type
data SParser ss sr p where
    SParser
        :: SParserChSym ss sr pCh
        -> SParserEndSym ss sr pEnd
        -> ss sInit
        -> SParser ss sr ('Parser pCh pEnd sInit)

type SParserChSym ss sr f = Lam2 SChar ss (SResult ss sr) f
type SParserChSym1 ch ss sr f = SChar ch -> Lam ss (SResult ss sr) (f ch)
type SParserEndSym ss sr f = Lam ss (SEither SE sr) f

-- | The result of a single step of a parser.
--
-- Promotable. Instantiate with 'String' for term-level, 'Symbol' for
-- type-level.
--
-- No reifying provided as we have no general way to reify @s@ state and @r@
-- result types.
data Result str s r
  = Cont s       -- ^ OK, continue with the given state
  | Done r       -- ^ OK, parse successful with result @r@
  | Err  (E str) -- ^ parse error

data SResult (ss :: s -> Type) (sr :: r -> Type) (res :: PResult s r) where
    SCont :: ss s -> SResult ss sr (Cont s)
    SDone :: sr r -> SResult ss sr (Done r)
    SErr  :: SE e -> SResult ss sr (Err e)

-- | Parse a 'Char' with the given state.
--
-- The action is always consuming. For this reason, you may need to look ahead
-- for the final case, so as to not consume an extra 'Char'. This prevents many
-- zero-length parsers. It's a bit weird. See
-- 'Data.Type.Symbol.Parser.Parser.Drop' for an example.
type ParserCh str s r = Char -> s -> Result str s r

-- | A defunctionalization symbol for a 'ParserCh'.
type ParserChSym s r = Char ~> s ~> PResult s r

-- | A partially-applied defunctionalization symbol for a 'ParserCh'.
type ParserChSym1 s r = Char -> s ~> PResult s r

-- | What a parser should do at the end of a 'Symbol'.
type ParserEnd str s r = s -> Either (E str) r

-- | A defunctionalization symbol for a 'ParserEnd'.
type ParserEndSym s r = s ~> Either PE r

-- | Promoted 'ParserCh'.
type PParserCh s r = ParserCh Symbol s r

-- | Promoted 'ParserEnd'.
type PParserEnd s r = ParserEnd Symbol s r

-- | Promoted 'Result'.
type PResult = Result Symbol

-- | Parser error.
--
-- Promotable. Instantiate with 'String' for term-level, 'Symbol' for
-- type-level.
--
-- Reifiable (see 'ReifyE'). Also easily singleton-d, if you like that sort of
-- thing.
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

-- | Reify a promoted 'E' to its term-level equivalent.
class ReifyE (e :: E Symbol) where reifyE :: E String
instance (SingDoc doc, KnownSymbol name) => ReifyE (EBase name doc) where
    reifyE = EBase (symbolVal' (proxy# @name)) (reifyDoc @doc)
instance (ReifyE e, KnownSymbol name) => ReifyE (EIn name e) where
    reifyE = EIn (symbolVal' (proxy# @name)) (reifyE @e)

data SE (e :: PE) where
    SEBase :: SSymbol name -> SDoc doc -> SE (EBase name doc)
    SEIn   :: SSymbol name -> SE   e   -> SE (EIn name e)

class SingE (e :: PE) where singE :: SE e
instance (KnownSymbol name, SingDoc doc) => SingE (EBase name doc) where
    singE = SEBase (SSymbol @name) (singDoc @doc)
instance (KnownSymbol name, SingE e) => SingE (EIn name e) where
    singE = SEIn   (SSymbol @name) (singE   @e)

demoteSE :: SE e -> E String
demoteSE = \case
  SEBase sname sdoc -> EBase (fromSSymbol sname) (demoteDoc sdoc)
  SEIn   sname se   -> EIn   (fromSSymbol sname) (demoteSE  se)

type SResultEnd = SEither SE

withSingE :: forall e r. SE e -> (SingE e => r) -> r
withSingE = withDict @(SingE e)
