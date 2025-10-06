{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.End where

import Symparsec2.Parser.Common

-- | Assert end of input, or fail.
type End :: PParserSym ()
data End s
type instance App End s = End' (UnconsState s)
type family End' ms where
    End' '(Nothing,  s) = 'Reply (OK '()) s
    End' '(Just _ch, s) = 'Reply (Err EEof) s

type EEof = Error1 "expected end of string"
