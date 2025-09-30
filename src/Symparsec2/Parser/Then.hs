{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Then where

import Symparsec2.Parser.Common
import DeFun.Core ( type (@@) )

type Then :: PParserSym a -> PParserSym b -> PParserSym (a, b)
data Then l r s
type instance App (Then l r) s = ThenL r (l @@ s)
type family ThenL r res where
    ThenL r ('Result (Left  e) s) = Err s (EIn "Then(L)" e)
    ThenL r ('Result (Right a) s) = ThenR a (r @@ s)
type family ThenR a res where
    ThenR a ('Result (Left  e) s) = Err s (EIn "Then(R)" e)
    ThenR a ('Result (Right b) s) = Done s '(a, b)
