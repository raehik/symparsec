{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.While where

import Symparsec2.Parser.Common

type While :: (Char ~> Bool) -> PParser r -> PParser Natural
data While chPred p s
type instance App (While chPred p) s = While' chPred p s

type family While' chPred p s where
    While' chPred p ('State rem len idx) =
        WhileCountStart len rem idx chPred p (UnconsSymbol rem)

type family WhileCountStart len rem idx chPred p mstr where
    WhileCountStart len rem idx chPred p (Just '(ch, str)) =
        WhileCount len rem idx chPred p 0 (UnconsSymbol str) (chPred @@ ch)
    WhileCountStart len rem idx chPred p Nothing           = p @@ ('State rem 0 idx)

type family WhileCount len rem idx chPred p n mstr res where
    WhileCount len rem idx chPred p n (Just '(ch, str)) True  =
        WhileCount len rem idx chPred p (n+1) (UnconsSymbol str) (chPred @@ ch)
    WhileCount len rem idx chPred p n (Just '(ch, str)) False =
        WhileEnd (len-n)     (p @@ ('State rem n     idx))
    WhileCount len rem idx chPred p n Nothing           True  =
        WhileEnd (len-(n+1)) (p @@ ('State rem (n+1) idx))
    WhileCount len rem idx chPred p n Nothing           False =
        WhileEnd (len-n)     (p @@ ('State rem n     idx))

type family WhileEnd lenRest res where
    WhileEnd lenRest ('Reply res ('State rem len idx)) =
        'Reply res ('State rem (lenRest+len) idx)
