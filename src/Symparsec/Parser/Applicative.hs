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
type (<*>) :: PParser s (a ~> b) -> PParser s a -> PParser s b
infixl 4 <*>
data (<*>) l r ps
type instance App (l <*> r) ps = ApL r (l @@ ps)
type ApL :: PParser s a -> PReply s (a ~> b) -> PReply s b
type family ApL r rep where
    ApL r ('Reply (OK  fa) ps) = (fa <$> r) @@ ps
    ApL r ('Reply (Err e)  ps) = 'Reply (Err e) ps

-- | 'pure' for parsers. Non-consuming parser that just returns the given value.
type Pure :: forall s a. a -> PParser s a
data Pure a ps
type instance App (Pure a) ps = 'Reply (OK a) ps

-- | 'liftA2' for parsers. Sequence two parsers, and combine their results with
-- a binary type function.
type LiftA2
    :: forall s a b c
    .  (a ~> b ~> c) -> PParser s a -> PParser s b -> PParser s c
type LiftA2 f l r = (f <$> l) <*> r

-- | '*>' for parsers. Sequence two parsers left to right, discarding the value
-- of the left parser.
type (*>) :: PParser s a -> PParser s b -> PParser s b
infixl 4 *>
type l *> r = (IdSym <$ l) <*> r

-- | '<*' for parsers. Sequence two parsers left to right, discarding the value
-- of the right parser.
type (<*) :: PParser s a -> PParser s b -> PParser s a
infixl 4 <*
type l <* r = LiftA2 ConstSym l r
