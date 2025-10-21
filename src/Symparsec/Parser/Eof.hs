{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Eof ( type Eof ) where

import Symparsec.Parser.Common

-- | Assert end of input, or fail.
type Eof :: PParser s ()
data Eof ps
type instance App Eof ps = Eof' (UnconsState ps)
type family Eof' mps where
    Eof' '(Nothing,  ps) = 'Reply (OK '()) ps
    Eof' '(Just _ch, ps) = 'Reply (Err EEof) ps

type EEof = Error1 "expected end of string"
