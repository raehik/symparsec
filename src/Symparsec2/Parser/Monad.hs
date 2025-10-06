{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Monad' functions.

module Symparsec2.Parser.Monad ( type (>>=) ) where

import Symparsec2.Parser.Common

-- | '>>=' for parsers. Sequentially compose two parsers, passing the value from
-- the left parser as an argument to the second.
type (>>=) :: PParser a -> (a ~> PParser b) -> PParser b
infixl 1 >>=
data (>>=) l r s
type instance App (l >>= r) s = BindL r (l @@ s)
type BindL :: (a ~> PParser b) -> PReply a -> PReply b
type family BindL r rep where
    BindL r ('Reply (OK  a) s) = r @@ a @@ s
    BindL r ('Reply (Err e) s) = 'Reply (Err e) s
