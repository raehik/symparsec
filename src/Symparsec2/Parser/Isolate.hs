{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Isolate where

import Symparsec2.Parser.Common
import Symparsec2.Utils ( type IfNatLte )

type Isolate :: Natural -> PParserSym r -> PParserSym r
data Isolate n p s
type instance App (Isolate n p) s = Isolate' n p s
type family Isolate' n p s where
    Isolate' n p ('State rem len idx) =
        -- Could perhaps improve this, since 'OrdCond' probably does similar
        -- work to @len-n@.
        IfNatLte n len
            (IsolateEnd len n (p @@ ('State rem n idx)))
            (Err ('State rem len idx) EIsolateOverlong)

--type IsolateEnd :: Natural -> ? -> ?
-- TODO are lenRem/lenConsumed actually good names?
type family IsolateEnd lenOrig n res where
    -- isolated parser succeeded and consumed all input:
    -- return success with state updated to have actual remaining length
    IsolateEnd lenOrig n ('Result (Right r) ('State rem 0   idx)) =
        Done ('State rem (lenOrig-n) idx) r

    -- isolated parser failed
    IsolateEnd lenOrig n ('Result (Left  e) ('State rem len idx)) =
        Err  ('State rem (lenOrig-n+len) idx) (EIn "Isolate" e)

    -- isolated parser succeeded but didn't consume all input
    IsolateEnd lenOrig n ('Result (Right r) ('State rem len idx)) =
        Err  ('State rem (lenOrig-n+len) idx) (EIsolateRemaining len)

type EIsolateOverlong =
    EBase "Isolate" (Text "TODO tried to isolate past input")

type EIsolateRemaining n = EBase "Isolate"
    (      Text "isolated parser completed without consuming all input ("
      :<>: Text (ShowNatDec n) :<>: Text " remaining)" )
