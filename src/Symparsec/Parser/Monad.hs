{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Monad' functions.

module Symparsec.Parser.Monad ( type (>>=) ) where

import Symparsec.Parser.Common

-- | '>>=' for parsers. Sequentially compose two parsers, passing the value from
-- the left parser as an argument to the second.
type (>>=) :: PParser s a -> (a ~> PParser s b) -> PParser s b
infixl 1 >>=
data (>>=) l r ps
type instance App (l >>= r) ps = BindL r (l @@ ps)
type BindL :: (a ~> PParser s b) -> PReply s a -> PReply s b
type family BindL r rep where
    BindL r ('Reply (OK  a) ps) = r @@ a @@ ps
    BindL r ('Reply (Err e) ps) = 'Reply (Err e) ps
