{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Alternative' functions.

module Symparsec.Parser.Alternative
  ( type (<|>), type Empty
  , type Optional
  ) where

import Symparsec.Parser.Functor
import Symparsec.Parser.Applicative
import Symparsec.Parser.Common
import DeFun.Core

-- | 'Control.Alternative.<|>' for parsers. Try the left parser; if it succeeds, return the result,
-- else try the right parser with the left parser's output state.
--
-- Does not backtrack. Wrap parsers with 'Symparsec.Parser.Try' as needed.
--
-- TODO shitty errors
type (<|>) :: PParser a -> PParser a -> PParser a
infixl 3 <|>
data (<|>) l r s
type instance App (l <|> r) s = Plus r (l @@ s)
type Plus :: PParser a -> PReply a -> PReply a
type family Plus r rep where
    Plus r ('Reply (OK   a) s) = 'Reply (OK a) s
    Plus r ('Reply (Err _e) s) = r @@ s

-- | 'Control.Alternative.empty' for parsers. Immediately fail with no consumption.
type Empty :: PParser a
data Empty s
type instance App Empty s = 'Reply (Err (Error1 "called empty parser")) s

-- | 'Control.Alternative.optional' for parsers.
type Optional :: PParser a -> PParser (Maybe a)
type Optional p = Con1 Just <$> p <|> Pure Nothing

{- Wow, I guess that works. But also, the manual version:
data Optional p s
type instance App (Optional p) s = OptionalEnd (p @@ s)

type family OptionalEnd rep where
    OptionalEnd ('Reply (OK   a) s) = 'Reply (OK (Just a)) s
    OptionalEnd ('Reply (Err _e) s) = 'Reply (OK Nothing)  s
-}
