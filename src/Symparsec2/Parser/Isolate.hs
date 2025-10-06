{-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Parser.Isolate where

import Symparsec2.Parser.Common
import Symparsec2.Utils ( type IfNatLte )

-- TODO can use 'Ensure' to help define this
type Isolate :: Natural -> PParser r -> PParser r
data Isolate n p s
type instance App (Isolate n p) s = Isolate' n p s
type family Isolate' n p s where
    Isolate' n p ('State rem len idx) =
        -- Could perhaps improve this, since 'OrdCond' probably does similar
        -- work to @len-n@.
        IfNatLte n len
            (IsolateEnd len n (p @@ ('State rem n idx)))
            ('Reply (Err (Error1 (EStrInputTooShort n len))) ('State rem len idx))

--type IsolateEnd :: Natural -> ? -> ?
-- TODO are lenRem/lenConsumed actually good names?
type family IsolateEnd lenOrig n rep where
    -- isolated parser succeeded and consumed all input:
    -- return success with state updated to have actual remaining length
    IsolateEnd lenOrig n ('Reply (OK  a) ('State rem 0   idx)) =
        'Reply (OK  a) ('State rem (lenOrig-n)     idx)

    -- isolated parser failed
    IsolateEnd lenOrig n ('Reply (Err e) ('State rem len idx)) =
        -- TODO add some isolate meta
        'Reply (Err e) ('State rem (lenOrig-n+len) idx)

    -- isolated parser succeeded but didn't consume all input
    IsolateEnd lenOrig n ('Reply (OK _a) ('State rem len idx)) =
        'Reply (Err (EIsolateIncomplete len)) ('State rem (lenOrig-n+len) idx)

type EIsolateIncomplete n = Error1
    (    "isolated parser completed without consuming all input ("
      ++ ShowNatDec n ++ " remaining)" )

-- TODO testing
type IsolateSym :: PParser r -> Natural ~> PParser r
data IsolateSym p x
type instance App (IsolateSym p) n = Isolate n p
