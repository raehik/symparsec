{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Applicative' functions.

module Symparsec.Parser.Applicative
  ( type (<*>), type Pure
  , type LiftA2
  , type (*>), type (<*)
  ) where

import Symparsec.Parser.Common
import Symparsec.Parser.Functor
import DeFun.Function ( type IdSym, type ConstSym )

-- | '<*>' for parsers. Sequence two parsers, left to right.
type (<*>) :: PParser u (a ~> b) -> PParser u a -> PParser u b
infixl 4 <*>
data (<*>) l r s
type instance App (l <*> r) s = ApL r (l @@ s)
type ApL :: PParser u a -> PReply u (a ~> b) -> PReply u b
type family ApL r rep where
    ApL r ('Reply (OK  fa) s) = (fa <$> r) @@ s
    ApL r ('Reply (Err e)  s) = 'Reply (Err e) s

-- | 'pure' for parsers. Non-consuming parser that just returns the given value.
type Pure :: a -> PParser u a
data Pure a s
type instance App (Pure a) s = 'Reply (OK a) s

-- | 'liftA2' for parsers. Sequence two parsers, and combine their results with
-- a binary type function.
type LiftA2 :: (a ~> b ~> c) -> PParser u a -> PParser u b -> PParser u c
type LiftA2 f l r = (f <$> l) <*> r

-- | '*>' for parsers. Sequence two parsers left to right, discarding the value
-- of the left parser.
type (*>) :: PParser u a -> PParser u b -> PParser u b
infixl 4 *>
type l *> r = (IdSym <$ l) <*> r

-- | '<*' for parsers. Sequence two parsers left to right, discarding the value
-- of the right parser.
type (<*) :: PParser u a -> PParser u b -> PParser u a
infixl 4 <*
type l <* r = LiftA2 ConstSym l r
