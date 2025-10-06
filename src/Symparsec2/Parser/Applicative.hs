{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Applicative' functions.

module Symparsec2.Parser.Applicative
  ( type (<*>), type LiftA2, type (*>), type (<*)
  , type Pure
  ) where

import Symparsec2.Parser.Common
import Symparsec2.Parser.Functor
import DeFun.Function ( type IdSym, type ConstSym )

-- | '<*>' for parsers. Sequence two parsers, left to right.
type (<*>) :: PParserSym (a ~> b) -> PParserSym a -> PParserSym b
infixl 4  <*>
data (<*>) l r s
type instance App (l <*> r) s = ThenL r (l @@ s)
type ThenL :: PParserSym a -> PReply (a ~> b) -> PReply b
type family ThenL r res where
    ThenL r ('Reply (OK  fa) s) = (fa <$> r) @@ s
    ThenL r ('Reply (Err e)  s) = 'Reply (Err e) s

-- | 'liftA2' for parsers. Sequence two parsers, and combine their results with
-- a binary type function.
type LiftA2 :: (a ~> b ~> c) -> PParserSym a -> PParserSym b -> PParserSym c
type LiftA2 f l r = (f <$> l) <*> r

-- | '*>' for parsers. Sequence two parsers left to right, discarding the value
-- of the left parser.
type (*>) :: PParserSym a -> PParserSym b -> PParserSym b
infixl 4 *>
type l *> r = (IdSym <$ l) <*> r

-- | '<*' for parsers. Sequence two parsers left to right, discarding the value
-- of the right parser.
type (<*) :: PParserSym a -> PParserSym b -> PParserSym a
infixl 4 <*
type l <* r = LiftA2 ConstSym l r

-- | 'pure' for parsers. Non-consuming parser that just returns the given value.
type Pure :: a -> PParserSym a
data Pure a s
type instance App (Pure a) s = 'Reply (OK a) s
