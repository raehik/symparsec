{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Alternative' functions.

module Symparsec.Parser.Alternative
  ( type (<|>), type Empty
  , type Optional
  , type Many, type Some
  , type SepBy, type SepBy1
  , type Choice
  ) where

import Symparsec.Parser.Functor
import Symparsec.Parser.Applicative
import Symparsec.Parser.Common
import DeFun.Core
import qualified Singleraeh.List as List

-- | 'Control.Alternative.<|>' for parsers. Try the left parser; if it succeeds, return the result,
-- else try the right parser with the left parser's output state.
--
-- Does not backtrack. Wrap parsers with 'Symparsec.Parser.Try' as needed.
--
-- TODO shitty errors
type (<|>) :: PParser u a -> PParser u a -> PParser u a
infixl 3 <|>
data (<|>) l r s
type instance App (l <|> r) s = Plus r (l @@ s)
type Plus :: PParser u a -> PReply u a -> PReply u a
type family Plus r rep where
    Plus r ('Reply (OK   a) s) = 'Reply (OK a) s
    Plus r ('Reply (Err _e) s) = r @@ s

-- | 'Control.Alternative.empty' for parsers. Immediately fail with no consumption.
type Empty :: PParser u a
data Empty s
type instance App Empty s = 'Reply (Err (Error1 "called empty parser")) s

-- | 'Control.Alternative.optional' for parsers.
type Optional :: PParser u a -> PParser u (Maybe a)
type Optional p = Con1 Just <$> p <|> Pure Nothing

{- Wow, I guess that works. But also, the manual version:
data Optional p s
type instance App (Optional p) s = OptionalEnd (p @@ s)

type family OptionalEnd rep where
    OptionalEnd ('Reply (OK   a) s) = 'Reply (OK (Just a)) s
    OptionalEnd ('Reply (Err _e) s) = 'Reply (OK Nothing)  s
-}

-- | 'Control.Alternative.many' for parsers.
--
-- Does not backtrack. Wrap parsers with 'Symparsec.Parser.Try' as needed.
type Many :: PParser u a -> PParser u [a]
data Many p s
type instance App (Many p) s = Many' p '[] (p @@ s)
type family Many' p as rep where
    Many' p as ('Reply (OK   a) s) = Many' p (a:as) (p @@ s)
    Many' p as ('Reply (Err _e) s) = 'Reply (OK (List.Reverse as)) s

-- | 'Control.Alternative.some' for parsers.
--
-- Does not backtrack. Wrap parsers with 'Symparsec.Parser.Try' as needed.
type Some :: PParser u a -> PParser u [a]
type Some p = LiftA2 (Con2 '(:)) p (Many p)

-- | @'SepBy' p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values parsed by @p@.
type SepBy :: PParser u a -> PParser u sep -> PParser u [a]
type SepBy p sep = SepBy1 p sep <|> Pure '[]

-- | @'SepBy1' p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values parsed by @p@.
type SepBy1 :: PParser u a -> PParser u sep -> PParser u [a]
type SepBy1 p sep = LiftA2 (Con2 '(:)) p (Many (sep *> p))

-- TODO doesn't backtrack, matching megaparsec.
type Choice :: [PParser u a] -> PParser u a
data Choice pList ps
type instance App (Choice pList) ps = ChoiceStart pList ps

type family ChoiceStart pList ps where
    ChoiceStart '[]       ps = Empty @@ ps
    ChoiceStart (p:pList) ps = ChoiceLoop pList (p @@ ps)

type family ChoiceLoop pList rep where
    ChoiceLoop _         ('Reply (OK  a) ps) = 'Reply (OK a) ps
    ChoiceLoop (p:pList) ('Reply (Err e) ps) = ChoiceLoop pList (p @@ ps)
    ChoiceLoop '[]       rep                 = rep -- TODO meh, not the best error
