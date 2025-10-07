{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Try ( type Try ) where

import Symparsec.Parser.Common

-- | Run the given parser, backtracking on error.
type Try :: PParser a -> PParser a
data Try p s
type instance App (Try p) s = Try' s (p @@ s)
type Try' :: PState -> PReply a -> PReply a
type family Try' sPrev rep where
    Try' sPrev ('Reply (OK  a) s) = 'Reply (OK  a) s
    Try' sPrev ('Reply (Err e) s) = 'Reply (Err e) sPrev
