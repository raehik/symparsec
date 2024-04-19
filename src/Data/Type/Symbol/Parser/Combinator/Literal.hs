{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Combinator.Literal ( Literal ) where

import Data.Type.Symbol.Parser.Types
import Data.Type.Symbol.Parser.Common ( FailChSym )
import GHC.TypeLits
import DeFun.Core ( type (~>), type App )

type Literal :: Symbol -> Parser (Maybe (Char, Symbol)) ()
type family Literal sym where
    Literal "" =
        '(FailChSym "cannot parse the empty literal due to parser limitations", LiteralEndSym, Nothing)
    Literal sym = '(LiteralChSym, LiteralEndSym, UnconsSymbol sym)

type LiteralCh :: ParserCh (Maybe (Char, Symbol)) ()
type family LiteralCh ch msym where
    LiteralCh ch (Just '(ch,  ""))  = Done '()
    LiteralCh ch (Just '(ch,  sym)) = Cont (UnconsSymbol sym)
    LiteralCh ch (Just '(ch', sym)) = Err
        (Text "expected " :<>: ShowType ch :<>: Text ", got " :<>: ShowType ch')

type LiteralEnd :: ParserEnd (Maybe (Char, Symbol)) ()
type family LiteralEnd msym where
    LiteralEnd Nothing = Right '()
    LiteralEnd (Just '(ch, sym)) = Left
      ( Text "still parsing literal: " :<>: Text (ConsSymbol ch sym))

type LiteralChSym :: ParserChSym (Maybe (Char, Symbol)) ()
data LiteralChSym f
type instance App LiteralChSym f = LiteralChSym1 f

type LiteralChSym1
    :: Char -> Maybe (Char, Symbol) ~> Result (Maybe (Char, Symbol)) ()
data LiteralChSym1 ch n
type instance App (LiteralChSym1 ch) n = LiteralCh ch n

type LiteralEndSym :: ParserEndSym (Maybe (Char, Symbol)) ()
data LiteralEndSym msym
type instance App LiteralEndSym msym = LiteralEnd msym
