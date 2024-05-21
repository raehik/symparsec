{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Literal ( Literal, Literal', Literal'' ) where

import Symparsec.Parser.Common
import GHC.TypeLits ( Symbol, UnconsSymbol, ConsSymbol )
import Singleraeh.Symbol ( ReconsSymbol )
import TypeLevelShow.Utils ( ShowChar )

-- literal state (it's a mouthful)
type LiteralS = (Char, Maybe (Char, Symbol))

type Literal'' :: Char -> Maybe (Char, Symbol) -> Parser LiteralS ()
type Literal'' ch msym = 'Parser LiteralChSym LiteralEndSym '(ch, msym)

-- | Parse the given 'Symbol'.
type Literal :: Symbol -> Parser LiteralS ()
type Literal sym = Literal' (UnconsSymbol sym)

type EEmptyLit = ErrParserLimitation "cannot parse empty literal"

type family Literal' msym where
    Literal' Nothing           = 'Parser
        (FailChSym "Literal" EEmptyLit)
        (FailEndSym "Literal" EEmptyLit)
        '( '\0', Nothing)
    Literal' (Just '(ch, sym)) = Literal'' ch (UnconsSymbol sym)

type family LiteralCh ch s where
    LiteralCh ch0 '(ch0, Just '(ch1, sym)) = Cont '(ch1, UnconsSymbol sym)
    LiteralCh ch0 '(ch0, Nothing)          = Done '()
    LiteralCh ch  '(ch0, msym)             = Err (EBase "Literal"
        (      Text "expected " :<>: Text (ShowChar ch0)
          :<>: Text    ", got " :<>: Text (ShowChar ch)))

type LiteralEnd :: PParserEnd LiteralS ()
type family LiteralEnd s where
    LiteralEnd '(ch0, msym) = Left (EBase "Literal"
      (      Text "still parsing literal: "
        :<>: Text (ConsSymbol ch0 (ReconsSymbol msym))))

type LiteralChSym :: ParserChSym LiteralS ()
data LiteralChSym f
type instance App LiteralChSym f = LiteralChSym1 f

type LiteralChSym1 :: ParserChSym1 LiteralS ()
data LiteralChSym1 ch s
type instance App (LiteralChSym1 ch) s = LiteralCh ch s

type LiteralEndSym :: ParserEndSym LiteralS ()
data LiteralEndSym s
type instance App LiteralEndSym s = LiteralEnd s
