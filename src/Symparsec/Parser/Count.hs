{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Count ( type Count ) where

import Symparsec.Parser.Common
import qualified Singleraeh.List as List

-- TODO Could possibly make more efficient.

-- | @'Count' n p@ parses @n@ occurrences of @p@.
type Count :: Natural -> PParser s a -> PParser s [a]
data Count n p ps
type instance App (Count n p) ps = CountLoop p '[] n ps

type family CountLoop p as n ps where
    CountLoop p as 0 ps = 'Reply (OK (List.Reverse as)) ps
    CountLoop p as n ps = CountLoopWrap p as n (p @@ ps)

type family CountLoopWrap p as n rep where
    CountLoopWrap p as n ('Reply (OK  a) ps) =
        CountLoop p (a:as) (n-1) ps
    CountLoopWrap p as n ('Reply (Err e) ps) =
        -- TODO am I passing the wrong state back here?
        'Reply (Err e) ps
