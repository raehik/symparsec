{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Skip ( type Skip, type SkipUnsafe ) where

import Symparsec.Parser.Common
import Symparsec.Parser.Ensure
import Symparsec.Parser.Applicative
import Data.Type.Symbol qualified as Symbol

-- | Skip forward @n@ characters. Fails if fewer than @n@ characters remain.
type Skip :: Natural -> PParser ()
type Skip n = Ensure n *> SkipUnsafe n

-- | Skip forward @n@ characters. @n@ must be less than or equal to the number
--   of remaining characters. (Fairly unhelpful; use 'Skip' instead.)
type SkipUnsafe :: Natural -> PParser ()
data SkipUnsafe n s
type instance App (SkipUnsafe n) s = SkipUnsafe' n s
type family SkipUnsafe' n s where
    SkipUnsafe' n ('State rem len idx) =
        'Reply (OK '()) ('State (Symbol.Drop n rem) (len-n) (idx+n))
