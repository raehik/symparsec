{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.While ( type While ) where

import Symparsec.Parser.Common

-- | Run the given parser while the given character predicate succeeds.
type While :: (Char ~> Bool) -> PParser s a -> PParser s a
data While chPred p ps
type instance App (While chPred p) ps = While' chPred p ps

type family While' chPred p ps where
    While' chPred p ('State s rem len idx) =
        WhileCountStart s len rem idx chPred p (UnconsSymbol rem)

type family WhileCountStart s len rem idx chPred p mstr where
    WhileCountStart s len rem idx chPred p (Just '(ch, str)) =
        WhileCount s len rem idx chPred p 0 (UnconsSymbol str) (chPred @@ ch)
    WhileCountStart s len rem idx chPred p Nothing           = p @@ ('State s rem 0 idx)

type family WhileCount s len rem idx chPred p n mstr res where
    WhileCount s len rem idx chPred p n (Just '(ch, str)) True  =
        WhileCount s len rem idx chPred p (n+1) (UnconsSymbol str) (chPred @@ ch)
    WhileCount s len rem idx chPred p n (Just '(ch, str)) False =
        WhileEnd (len-n)     (p @@ ('State s rem n     idx))
    WhileCount s len rem idx chPred p n Nothing           True  =
        WhileEnd (len-(n+1)) (p @@ ('State s rem (n+1) idx))
    WhileCount s len rem idx chPred p n Nothing           False =
        WhileEnd (len-n)     (p @@ ('State s rem n     idx))

type family WhileEnd lenRest rep where
    -- TODO note that we don't require that the inner parser fully consumes.
    -- that's because we "lie" about how this parser works. you probably want a
    -- sort of char-by-char parser, but we measure a chunk and pass that.
    -- but by not requiring full consumption, we recover char-by-char behaviour!
    -- and we can still get full consumption by combining with Isolate.
    -- the inner parser should generally fully consume though, as a design point
    WhileEnd lenRest ('Reply res ('State s rem len idx)) =
        'Reply res ('State s rem (lenRest+len) idx)
