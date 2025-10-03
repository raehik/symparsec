{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Then where

import Symparsec2.Parser.Common

-- | Sequence two parsers, running left then right, and return both results.
type (:<*>:) :: PParserSym a -> PParserSym b -> PParserSym (a, b)
data (:<*>:) l r s
type instance App (l :<*>: r) s = ThenL r (l @@ s)
type ThenL :: PParserSym b -> PResult a -> PResult (a, b)
type family ThenL r res where
    ThenL r ('Result (Right a) s) = ThenR a (r @@ s)
    ThenL r ('Result (Left  e) s) = Err s (EIn "Then(L)" e)
type ThenR :: a -> PResult b -> PResult (a, b)
type family ThenR a res where
    ThenR a ('Result (Right b) s) = Done s '(a, b)
    ThenR a ('Result (Left  e) s) = Err s (EIn "Then(R)" e)

-- | Sequence two parsers, running left then right, and return only the right
--   result (discarding the left).
type (:*>:) :: PParserSym a -> PParserSym b -> PParserSym b
data (:*>:) l r s
type instance App (l :*>: r) s = ThenVLL r (l @@ s)
type ThenVLL :: PParserSym b -> PResult b -> PResult b
type family ThenVLL r res where
    ThenVLL r ('Result (Right _a) s) = ThenVLR (r @@ s)
    ThenVLL r ('Result (Left   e) s) = Err s (EIn "ThenVL(L)" e)
type ThenVLR :: PResult b -> PResult b
type family ThenVLR res where
    ThenVLR ('Result (Right b) s) = Done s b
    ThenVLR ('Result (Left  e) s) = Err s (EIn "ThenVL(R)" e)
