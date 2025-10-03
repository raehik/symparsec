{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.TakeRest where

import Symparsec2.Parser.Common
import qualified Data.Type.Symbol as Symbol

-- | Return the remaining input string.
type TakeRest :: PParserSym Symbol
data TakeRest s
type instance App TakeRest s = TakeRest' s
type family TakeRest' s where
    TakeRest' ('State rem len idx) =
        Done ('State (Symbol.Drop len rem) 0 (idx+len)) (Symbol.Take len rem)
