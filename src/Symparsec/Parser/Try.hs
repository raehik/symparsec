{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Try ( type Try ) where

import Symparsec.Parser.Common

-- | Run the given parser, backtracking on error.
type Try :: PParser s a -> PParser s a
data Try p ps
type instance App (Try p) ps = Try' ps (p @@ ps)
type Try' :: PState s -> PReply s a -> PReply s a
type family Try' psPrev rep where
    Try' psPrev ('Reply (OK  a) ps) = 'Reply (OK  a) ps
    Try' psPrev ('Reply (Err e) ps) = 'Reply (Err e) psPrev
