{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Eof ( type Eof ) where

import Symparsec.Parser.Common

-- | Assert end of input, or fail.
type Eof :: PParser ()
data Eof s
type instance App Eof s = Eof' (UnconsState s)
type family Eof' ms where
    Eof' '(Nothing,  s) = 'Reply (OK '()) s
    Eof' '(Just _ch, s) = 'Reply (Err EEof) s

type EEof = Error1 "expected end of string"
