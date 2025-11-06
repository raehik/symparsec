{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Satisfy ( type Satisfy, type OneOf, type NoneOf ) where

import Symparsec.Parser.Common

-- may also be defined using @Token@
type Satisfy :: (Char ~> Bool) -> PParser s Char
data Satisfy chPred ps
type instance App (Satisfy chPred) ps = SatisfyStart chPred ps (UnconsState ps)

type family SatisfyStart chPred psPrev mps where
    SatisfyStart chPred psPrev '(Just ch, ps) = SatisfyValidate psPrev ps ch (chPred @@ ch)
    SatisfyStart chPred psPrev '(Nothing, ps) =
        'Reply (Err (Error1 "expected at least 1 char")) ps

type family SatisfyValidate psPrev ps ch res where
    SatisfyValidate psPrev ps ch True  = 'Reply (OK ch) ps
    SatisfyValidate psPrev ps ch False =
        'Reply (Err (Error1 "satisfy: char failed predicate")) psPrev

type OneOf :: [Char] -> PParser s Char
type OneOf chs = Satisfy (ElemSym chs)

-- TODO put in singleraeh
type Elem :: a -> [a] -> Bool
type family Elem a as where
    Elem _ '[]    = False
    Elem a (a:_)  = True
    Elem a (_:as) = Elem a as

-- NOTE: flipped from "normal" Elem
type ElemSym :: [a] -> a ~> Bool
data ElemSym as a
type instance App (ElemSym as) a = Elem a as

-- may also be defined using @CompSym2 NotSym (ElemSym chs)@
type NoneOf :: [Char] -> PParser s Char
--type NoneOf chs = Satisfy (CompSym2 NotSym (ElemSym chs))
type NoneOf chs = Satisfy (NotElemSym chs)

type NotElem :: a -> [a] -> Bool
type family NotElem a as where
    NotElem _ '[]    = True
    NotElem a (a:_)  = False
    NotElem a (_:as) = NotElem a as

-- NOTE: flipped from "normal" Elem
type NotElemSym :: [a] -> a ~> Bool
data NotElemSym as a
type instance App (NotElemSym as) a = NotElem a as
