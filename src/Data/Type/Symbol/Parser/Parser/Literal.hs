{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Parser.Literal ( Literal ) where

import Data.Type.Symbol.Parser.Parser
import Data.Type.Symbol.Parser.Parser.Common
import GHC.TypeLits
import DeFun.Core ( type (~>), type App )

-- | Parse the given 'Symbol'.
type Literal :: Symbol -> Parser (Char, Maybe (Char, Symbol)) ()
type Literal sym = Literal' (UnconsSymbol sym)

type EEmptyLit = ErrParserLimitation "cannot parse empty literal"

type family Literal' msym where
    Literal' Nothing           =
        '( FailChSym "Literal" EEmptyLit
         , FailEndSym "Literal" EEmptyLit, '( '\0', Nothing))
    Literal' (Just '(ch, sym)) =
        '(LiteralChSym, LiteralEndSym, '(ch, UnconsSymbol sym))

type family LiteralCh ch s where
    LiteralCh ch0 '(ch0, Just '(ch1, sym)) = Cont '(ch1, UnconsSymbol sym)
    LiteralCh ch0 '(ch0, Nothing)          = Done '()
    LiteralCh ch  '(ch0, msym)             = Err (EBase "Literal"
        (      Text "expected " :<>: ShowType ch0
          :<>: Text ", got " :<>: ShowType ch))

type LiteralEnd :: ParserEnd (Char, Maybe (Char, Symbol)) ()
type family LiteralEnd s where
    LiteralEnd '(ch0, msym) = Left (EBase "Literal"
      (      Text "still parsing literal: "
        :<>: Text (ConsSymbol ch0 (ReconsSymbol msym))))

type family ReconsSymbol msym where
    ReconsSymbol Nothing           = ""
    ReconsSymbol (Just '(ch, sym)) = ConsSymbol ch sym

type LiteralChSym :: ParserChSym (Char, Maybe (Char, Symbol)) ()
data LiteralChSym f
type instance App LiteralChSym f = LiteralChSym1 f

type LiteralChSym1
    :: Char
    -> (Char, Maybe (Char, Symbol))
    ~> Result (Char, Maybe (Char, Symbol)) ()
data LiteralChSym1 ch s
type instance App (LiteralChSym1 ch) s = LiteralCh ch s

type LiteralEndSym :: ParserEndSym (Char, Maybe (Char, Symbol)) ()
data LiteralEndSym s
type instance App LiteralEndSym s = LiteralEnd s
