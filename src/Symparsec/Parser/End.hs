{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.End where

import Symparsec.Parser.Common

-- | Assert end of input, or fail.
type End :: PParser ()
data End s
type instance App End s = End' (UnconsState s)
type family End' ms where
    End' '(Nothing,  s) = 'Reply (OK '()) s
    End' '(Just _ch, s) = 'Reply (Err EEof) s

type EEof = Error1 "expected end of string"
