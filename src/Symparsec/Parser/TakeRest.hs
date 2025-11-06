{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.TakeRest ( type TakeRest ) where

import Symparsec.Parser.Common
import qualified Data.Type.Symbol as Symbol

-- | Consume and return the rest of the input string.
--
-- Never fails. May return the empty string.
type TakeRest :: PParser s Symbol
data TakeRest ps
type instance App TakeRest ps = TakeRest' ps
type family TakeRest' ps where
    TakeRest' ('State s rem len idx) =
        'Reply (OK (Symbol.Take len rem)) ('State s (Symbol.Drop len rem) 0 (idx+len))

{-
import GHC.TypeLits
import DeFun.Core

sTakeRest :: SParser SSymbol TakeRest
sTakeRest = Lam $ \(SState srem slen sidx) ->
        SReply (SOK _) (SState _ (SNat @0) _)
-}
