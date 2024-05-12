{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Internal.List where

-- | Reverse a type level list.
type Reverse as = Reverse' as '[]
type family Reverse' (as :: [k]) (acc :: [k]) :: [k] where
  Reverse' '[]      acc = acc
  Reverse' (a : as) acc = Reverse' as (a : acc)
