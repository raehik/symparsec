{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Isolate ( type Isolate, type IsolateSym ) where

import Symparsec.Parser.Common
import Symparsec.Utils ( type IfNatLte )

-- TODO can use 'Ensure' to help define this
type Isolate :: Natural -> PParser s a -> PParser s a
data Isolate n p ps
type instance App (Isolate n p) ps = Isolate' n p ps
type family Isolate' n p ps where
    Isolate' n p ('State s rem len idx) =
        -- Could perhaps improve this, since 'OrdCond' probably does similar
        -- work to @len-n@.
        IfNatLte n len
            (IsolateEnd len n (p @@ ('State s rem n idx)))
            ('Reply (Err (Error1 (EStrInputTooShort n len))) ('State s rem len idx))

--type IsolateEnd :: Natural -> ? -> ?
-- TODO are lenRem/lenConsumed actually good names?
type family IsolateEnd lenOrig n rep where
    -- isolated parser succeeded and consumed all input:
    -- return success with state updated to have actual remaining length
    IsolateEnd lenOrig n ('Reply (OK  a) ('State s rem 0   idx)) =
        'Reply (OK  a) ('State s rem (lenOrig-n)     idx)

    -- isolated parser failed
    IsolateEnd lenOrig n ('Reply (Err e) ('State s rem len idx)) =
        -- TODO add some isolate meta
        'Reply (Err e) ('State s rem (lenOrig-n+len) idx)

    -- isolated parser succeeded but didn't consume all input
    IsolateEnd lenOrig n ('Reply (OK _a) ('State s rem len idx)) =
        'Reply (Err (EIsolateIncomplete len)) ('State s rem (lenOrig-n+len) idx)

type EIsolateIncomplete n = Error1
    (    "isolated parser completed without consuming all input ("
      ++ ShowNatDec n ++ " remaining)" )

-- TODO testing. args flipped because you're more likely to defun the len
type IsolateSym :: PParser s a -> Natural ~> PParser s a
data IsolateSym p x
type instance App (IsolateSym p) n = Isolate n p
