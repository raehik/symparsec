{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Apply where

import Symparsec2.Parser.Common

-- | Apply the given type function to the result.
--
-- Effectively 'fmap' for parsers.
--
-- Meta-parser: does no actual parsing work, does not wrap errors.
type (:<$>:) :: (r ~> r') -> PParserSym r -> PParserSym r'
data (:<$>:) f p s
type instance App (f :<$>: p) s = ApplyEnd f (p @@ s)

type family ApplyEnd f res where
    ApplyEnd f ('Result (Right r) s) =
        'Result (Right (f @@ r)) s
    ApplyEnd f ('Result (Left  e) s) =
        -- don't wrap error as we're not actually doing any work here
        'Result (Left e) s
