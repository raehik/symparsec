{-# LANGUAGE UndecidableInstances #-}

-- | Type-level string parsers shaped like 'Functor' functions.

module Symparsec.Parser.Functor
  ( type (<$>), type (<$), type ($>)
  ) where

import Symparsec.Parser.Common
import DeFun.Function ( type ConstSym1 )

-- | '<$>' for parsers. Apply the given type function to the result.
type (<$>) :: (a ~> b) -> PParser s a -> PParser s b
infixl 4 <$>
data (<$>) f p ps
type instance App (f <$> p) ps = FmapEnd f (p @@ ps)

type family FmapEnd f rep where
    FmapEnd f ('Reply (OK  a) ps) = 'Reply (OK  (f @@ a)) ps
    FmapEnd f ('Reply (Err e) ps) = 'Reply (Err e)        ps

-- | '<$' for parsers. Replace the parser result with the given value.
type (<$) :: a -> PParser s b -> PParser s a
infixl 4 <$
type a <$ p = ConstSym1 a <$> p

-- | 'Data.Functor.$>' for parsers. Flipped t'Symparsec.Parser.Functor.<$'.
type ($>) :: PParser s a -> b -> PParser s b
infixl 4 $>
type p $> a = ConstSym1 a <$> p
