{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Functor' functions.

module Symparsec2.Parser.Functor
  ( type (<$>), type (<$), type ($>)
  ) where

import Symparsec2.Parser.Common
import DeFun.Function ( type ConstSym1 )

-- | '<$>' for parsers. Apply the given type function to the result.
type (<$>) :: (a ~> b) -> PParser a -> PParser b
infixl 4 <$>
data (<$>) f p s
type instance App (f <$> p) s = FmapEnd f (p @@ s)

type family FmapEnd f res where
    FmapEnd f ('Reply (OK  a) s) = 'Reply (OK  (f @@ a)) s
    FmapEnd f ('Reply (Err e) s) = 'Reply (Err e)        s

-- | '<$' for parsers. Replace the parser result with the given value.
type (<$) :: a -> PParser b -> PParser a
infixl 4 <$
type a <$ p = ConstSym1 a <$> p

-- | '$>' for parsers. Flipped 'Symparsec2.Parser.Functor.<$'.
type ($>) :: PParser a -> b -> PParser b
infixl 4 $>
type p $> a = ConstSym1 a <$> p
