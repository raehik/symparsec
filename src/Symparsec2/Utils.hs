{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Utils ( type SymbolLength, type IfNatLte ) where

import GHC.TypeLits ( type Symbol, type UnconsSymbol )
import GHC.TypeNats ( type Natural, type (+) )

import GHC.TypeNats ( type CmpNat )
import Data.Type.Ord ( type OrdCond )

type SymbolLength  ::            Symbol               -> Natural
type SymbolLength' :: Natural -> Maybe (Char, Symbol) -> Natural
type SymbolLength str = SymbolLength' 0 (UnconsSymbol str)
type family SymbolLength' len mstr where
    SymbolLength' len Nothing            = len
    SymbolLength' len (Just '(_ch, str)) =
        SymbolLength' (len+1) (UnconsSymbol str)

type IfNatLte n m fThen fElse = OrdCond (CmpNat n m) fThen fThen fElse
