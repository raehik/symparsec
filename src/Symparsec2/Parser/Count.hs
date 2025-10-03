{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Count where

import Symparsec2.Parser.Common
import qualified Singleraeh.List as List

-- TODO Could possibly make more efficient.

-- | @'Count' n p@ parses @n@ occurrences of @p@.
type Count :: Natural -> PParserSym a -> PParserSym [a]
data Count n p s
type instance App (Count n p) s = CountLoop p '[] n s

type family CountLoop p as n s where
    CountLoop p as 0 s = Done s (List.Reverse as)
    CountLoop p as n s = CountLoopWrap p as n (p @@ s)

type family CountLoopWrap p as n res where
    CountLoopWrap p as n ('Result (Right a) s) =
        CountLoop p (a:as) (n-1) s
    CountLoopWrap p as n ('Result (Left  e) s) =
        Err s (EIn "Count" e)
