{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for reification

module Symparsec.Parser.Take ( Take, Take' ) where

import Symparsec.Parser.Common
import GHC.TypeLits ( Symbol, Natural, ConsSymbol, type (-) )
import TypeLevelShow.Natural ( ShowNatDec )

-- | Return the next @n@ characters.
type Take :: Natural -> ParserSym (Natural, [Char]) Symbol
type family Take n where
    Take 0 = 'ParserSym
        (FailChSym "Take" (ErrParserLimitation "can't take 0"))
        TakeEndSym
        '(0, '[])
    Take n = Take' n

-- | Unsafe 'Take' which doesn't check for @n=0@. May get stuck.
type Take' n = 'ParserSym TakeChSym TakeEndSym '(n, '[])

type TakeCh :: PParserCh (Natural, [Char]) Symbol
type family TakeCh ch s where
    TakeCh ch '(1, chs) = Done (RevCharsToSymbol (ch : chs))
    TakeCh ch '(n, chs) = Cont '(n-1, ch : chs)

type TakeEnd :: PParserEnd (Natural, [Char]) Symbol
type family TakeEnd s where
    TakeEnd '(0, chs) = Right (RevCharsToSymbol chs)
    TakeEnd '(n, _)   = Left (EBase "Take"
        (      Text "tried to take "
          :<>: Text (ShowNatDec n) :<>: Text " chars from empty string"))

type RevCharsToSymbol chs = RevCharsToSymbol' "" chs
type family RevCharsToSymbol' sym chs where
    RevCharsToSymbol' sym '[]        = sym
    RevCharsToSymbol' sym (ch : chs) = RevCharsToSymbol' (ConsSymbol ch sym) chs

type TakeChSym :: ParserChSym (Natural, [Char]) Symbol
data TakeChSym f
type instance App TakeChSym f = TakeChSym1 f

type TakeChSym1 :: Char -> (Natural, [Char]) ~> PResult (Natural, [Char]) Symbol
data TakeChSym1 ch s
type instance App (TakeChSym1 ch) s = TakeCh ch s

type TakeEndSym :: ParserEndSym (Natural, [Char]) Symbol
data TakeEndSym s
type instance App TakeEndSym s = TakeEnd s
