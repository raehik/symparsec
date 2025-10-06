{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.TakeRest where

import Symparsec2.Parser.Common
import qualified Data.Type.Symbol as Symbol

import GHC.TypeLits
import DeFun.Core

-- | Consume and return the rest of the input string.
--
-- Never fails. May return the empty string.
type TakeRest :: PParser Symbol
data TakeRest s
type instance App TakeRest s = TakeRest' s
type family TakeRest' s where
    TakeRest' ('State rem len idx) =
        'Reply (OK (Symbol.Take len rem)) ('State (Symbol.Drop len rem) 0 (idx+len))

--sTakeRest :: SParser SSymbol TakeRest
--sTakeRest = Lam $ \(SState srem slen sidx) ->
--        SReply (SOK _) (SState _ (SNat @0) _)
