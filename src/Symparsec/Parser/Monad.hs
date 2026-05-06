{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Monad' functions.

module Symparsec.Parser.Monad ( type (>>=) ) where

import Symparsec.Parser.Common

-- | '>>=' for parsers. Sequentially compose two parsers, passing the value from
-- the left parser as an argument to the second.
type (>>=) :: PParser u a -> (a ~> PParser u b) -> PParser u b
infixl 1 >>=
data (>>=) l r s
type instance App (l >>= r) s = BindL r (l @@ s)
type BindL :: (a ~> PParser u b) -> PReply u a -> PReply u b
type family BindL r rep where
    BindL r ('Reply (OK  a) s) = r @@ a @@ s
    BindL r ('Reply (Err e) s) = 'Reply (Err e) s
