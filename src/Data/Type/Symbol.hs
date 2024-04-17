{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol where

import GHC.TypeLits

-- | Get the length of a symbol.
type Length sym = Length' 0 (UnconsSymbol sym)

type family Length' len mchsym where
    Length' len 'Nothing          = len
    Length' len ('Just '(_, sym)) = Length' (len+1) (UnconsSymbol sym)
