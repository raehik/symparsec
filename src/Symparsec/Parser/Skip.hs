{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Skip ( type Skip, type SkipUnsafe ) where

import Symparsec.Parser.Common
import Symparsec.Parser.Ensure
import Symparsec.Parser.Applicative
import Data.Type.Symbol qualified as Symbol

-- | Skip forward @n@ characters. Fails if fewer than @n@ characters remain.
type Skip :: Natural -> PParser s ()
type Skip n = Ensure n *> SkipUnsafe n

-- | Skip forward @n@ characters. @n@ must be less than or equal to the number
--   of remaining characters. (Fairly unhelpful; use 'Skip' instead.)
type SkipUnsafe :: Natural -> PParser s ()
data SkipUnsafe n ps
type instance App (SkipUnsafe n) ps = SkipUnsafe' n ps
type family SkipUnsafe' n ps where
    SkipUnsafe' n ('State s rem len idx) =
        'Reply (OK '()) ('State s (Symbol.Drop n rem) (len-n) (idx+n))
