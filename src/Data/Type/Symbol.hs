{-# LANGUAGE UndecidableInstances #-}

-- | Type families on 'Symbol's.
module Data.Type.Symbol
  ( type Length
  , type Take, type TakeNoTailRec
  , type Replicate
  ) where

import GHC.TypeLits ( type Symbol, type UnconsSymbol, type ConsSymbol )
import GHC.TypeNats ( type Natural, type (+), type (-) )
import Singleraeh.Symbol ( type RevCharsToSymbol )

-- | Calculate the length of a 'Symbol'.
type Length  ::            Symbol               -> Natural
type Length' :: Natural -> Maybe (Char, Symbol) -> Natural
type Length str = Length' 0 (UnconsSymbol str)
type family Length' len mstr where
    Length' len Nothing            = len
    Length' len (Just '(_ch, str)) = Length' (len+1) (UnconsSymbol str)

-- | Take the prefix of the given 'Symbol' of the given length.
--
-- Returns less than requested if the 'Symbol' is too short.
type Take :: Natural -> Symbol -> Symbol
type Take n str = TakeLoop '[] n (UnconsSymbol str)
type family TakeLoop chs n mstr where
    TakeLoop chs 0 _                 = RevCharsToSymbol chs
    TakeLoop chs n (Just '(ch, str)) = TakeLoop (ch:chs) (n-1) (UnconsSymbol str)
    TakeLoop chs n Nothing           = RevCharsToSymbol chs

-- | Take the prefix of the given 'Symbol' of the given length.
--
-- Returns less than requested if the 'Symbol' is too short.
--
-- Does not do tail-call recursion, but avoids doing extra work.
-- Unsure which is better.
type TakeNoTailRec :: Natural -> Symbol -> Symbol
type TakeNoTailRec n str = TakeNoTailRec' n (UnconsSymbol str)
type family TakeNoTailRec' n mstr where
  TakeNoTailRec' 0 _                 = ""
  TakeNoTailRec' n (Just '(ch, str)) = ConsSymbol ch (TakeNoTailRec' (n-1) (UnconsSymbol str))
  TakeNoTailRec' _ Nothing           = ""

type Replicate :: Natural -> Char -> Symbol
type Replicate n ch = ReplicateLoop ch '[] n
type family ReplicateLoop ch chs n where
    ReplicateLoop ch chs 0 = RevCharsToSymbol chs
    ReplicateLoop ch chs n = ReplicateLoop ch (ch:chs) (n-1)
