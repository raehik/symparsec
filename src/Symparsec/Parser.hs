{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser where

import DeFun.Core
import GHC.TypeLits ( type Symbol )
import GHC.TypeNats ( type Natural )

import Singleraeh.Demote
import Data.Kind ( type Type )
import GHC.TypeLits ( type SSymbol, fromSSymbol )
import GHC.TypeNats ( type SNat, fromSNat )
import Singleraeh.List
--import Singleraeh.Symbol

-- | Parser state.
data State str n = State
  -- | Remaining input.
  { remaining :: str

  -- | Remaining permitted length.
  --
  -- Must be less than or equal to the actual length of the remaining input.
  -- Parsers must use this field when reading from input:
  --
  -- * if ==0, treat as end of input.
  -- * if  >0 but remaining input is empty, unrecoverable parser error
  --
  -- This extra bookkeeping permits much simpler parser design, specifically for
  -- parsers that act on a substring of the input.
  , length :: n

  -- | Index in the input string.
  --
  -- Overall index. Used for nicer error reporting after parse completion.
  , index :: n
  } deriving stock Show

-- | Promoted 'State'.
type PState = State Symbol Natural

-- | Singled 'State'.
data SState (s :: PState) where
    SState :: SSymbol rem -> SNat len -> SNat idx -> SState ('State rem len idx)

-- | Demote an 'SState'.
demoteSState :: SState s -> State String Natural
demoteSState (SState srem slen sidx) =
    State (fromSSymbol srem) (fromSNat slen) (fromSNat sidx)

instance Demotable SState where
    type Demote SState = State String Natural
    demote = demoteSState

{-
data Span n = Span
  { start :: n
  , end   :: n
  } deriving stock Show
-}

data Error str = Error
  { detail :: [str]
  } deriving stock Show

-- | Promoted 'Error'.
type PError = Error Symbol

-- | Singled 'Error'.
data SError (e :: PError) where
    SError :: SList SSymbol detail -> SError ('Error detail)

-- | Demote an 'SError'.
demoteSError :: SError e -> Error String
demoteSError (SError sdetail) = Error $ demoteSList fromSSymbol sdetail

instance Demotable SError where
    type Demote SError = Error String
    demote = demoteSError

-- | Parser completion: result, and final state.
--
-- TODO: megaparsec also returns a bool indicating if any input was consumed.
-- Unsure what it's used for.
data Reply str n a = Reply
  { result :: Result str n a -- ^ Parse result.
  , state  :: State str n    -- ^ Final parser state.
  } deriving stock Show

-- | Promoted 'Reply'.
type PReply = Reply Symbol Natural

-- | Singled 'Reply'.
data SReply (sa :: a -> Type) (rep :: PReply a) where
    SReply :: SResult sa result -> SState state -> SReply sa ('Reply result state)

-- | Demote an 'SReply.
demoteSReply
    :: (forall a. sa a -> da)
    -> SReply sa rep
    -> Reply String Natural da
demoteSReply demoteSA (SReply sresult sstate) =
    Reply (demoteSResult demoteSA sresult) (demoteSState sstate)

instance Demotable sa => Demotable (SReply sa) where
    type Demote (SReply sa) = Reply String Natural (Demote sa)
    demote = demoteSReply demote

-- | Parse result: a value, or an error.
data Result str n a = OK a            -- ^ Parser succeeded.
                    | Err (Error str) -- ^ Parser failed.
    deriving stock Show

-- | Promoted 'Result'.
type PResult = Result Symbol Natural

--type SState = State 
--type SResult :: _ -> Type
-- TODO ^ how to do explicit kind signature for GADT?

-- | Singled 'Result'.
data SResult (sa :: a -> Type) (res :: PResult a) where
    SOK  :: sa a     -> SResult sa (OK a)
    SErr :: SError e -> SResult sa (Err e)

-- | Demote an 'SResult'.
demoteSResult
    :: (forall a. sa a -> da)
    -> SResult sa res
    -> Result String Natural da
demoteSResult demoteSA = \case
  SOK  sa -> OK  $ demoteSA sa
  SErr se -> Err $ demoteSError se

instance Demotable sa => Demotable (SResult sa) where
    type Demote (SResult sa) = Result String Natural (Demote sa)
    demote = demoteSResult demote

-- | A parser is a function on parser state.
type Parser str n a = State str n -> Reply str n a

-- | Promoted 'Parser': a defunctionalization symbol to a function on promoted
--   parser state.
type PParser a = PState ~> PReply a

-- | Singled 'Parser'.
type SParser sa p = Lam SState (SReply sa) p
--data SParser (sa :: a -> Type) (p :: PParser a) where
 --   SParser :: Lam SState (SReply sa) (PParser a)

--class SingParser (p :: PParser a) where
--    singParser :: SParser sa p
