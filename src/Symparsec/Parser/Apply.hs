{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Apply where

import Symparsec.Parser.Common

-- | Apply the given type function to the result.
--
-- Effectively 'fmap' for parsers.
type (:<$>:) :: (r ~> r') -> PParser s r -> PParser s r'
type family f :<$>: p where
    f :<$>: 'PParser pCh pEnd s0 = Apply' f pCh pEnd s0

-- unwrapped for instances
type Apply' f pCh pEnd s0 = 'PParser (ApplyChSym f pCh) (ApplyEndSym f pEnd) s0

-- TODO: Singling is a pain, similar to While. Let's ignore it for now.

type ApplyCh
    :: (r ~> r')
    -> ParserChSym s r
    -> PParserCh s r'
type family ApplyCh f pCh ch s where
    ApplyCh f pCh ch s = ApplyCh' f (pCh @@ ch @@ s)

type family ApplyCh' f res where
    ApplyCh' f (Cont s) = Cont s
    ApplyCh' f (Done r) = Done (f @@ r)
    ApplyCh' f (Err  e) = Err e

type ApplyChSym
    :: (r ~> r')
    -> ParserChSym s r
    -> ParserChSym s r'
data ApplyChSym f pCh x
type instance App (ApplyChSym f pCh) x = ApplyChSym1 f pCh x

type ApplyChSym1
    :: (r ~> r')
    -> ParserChSym  s r
    -> ParserChSym1 s r'
data ApplyChSym1 f pCh ch s
type instance App (ApplyChSym1 f pCh ch) s = ApplyCh f pCh ch s

type ApplyEnd
    :: (r ~> r')
    -> ParserEndSym s r
    -> PParserEnd s r'
type family ApplyEnd f pEnd s where
    ApplyEnd f pEnd s = ApplyEnd' f (pEnd @@ s)

type family ApplyEnd' f res where
    ApplyEnd' f (Right r) = Right (f @@ r)
    ApplyEnd' f (Left  e) = Left  e

type ApplyEndSym
    :: (r ~> r')
    -> ParserEndSym s r
    -> ParserEndSym s r'
data ApplyEndSym f pEnd s
type instance App (ApplyEndSym f pEnd) s = ApplyEnd f pEnd s
