{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.End where

import Symparsec2.Parser.Common

-- | Assert end of input, or fail.
type End :: PParserSym ()
data End s
type instance App End s = End' (UnconsState s)
type family End' ms where
    End' '(Nothing,  s) = Done s '()
    End' '(Just _ch, s) = Err  s (EBase "End" (Text "expected end of string"))
