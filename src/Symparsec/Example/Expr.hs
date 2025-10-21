{-# LANGUAGE UndecidableInstances #-}

-- | An example Symparsec parser for a basic expression tree.

module Symparsec.Example.Expr where

import Symparsec.Parser.Common
import Symparsec.Utils ( type IfNatLte )
import Symparsec.Parser.Natural
import Symparsec.Parser.Natural.Digits
import Symparsec.Parser.While ( type While )
import Symparsec.Parser.While.Predicates ( type IsDecDigitSym )
import GHC.TypeNats qualified as TypeNats

{- TODO
* empty paren pairs are permitted in many cases e.g. @()1() -> ELit 1@
  * probably I should assert that >=1 thing gets parsed inside parens
  * well, I solved it. but I think in the wrong way. surely I should not be
    parsing parens in certain places. like I can't parse parens immediately
    after a number. or if I do, I need to implicitly parse a Mult.
-}

-- | A basic expression tree, polymorphic over a single literal type.
data Expr a
  = EBOp BOp (Expr a) (Expr a)
  | ELit a

-- | A binary operator.
data BOp = Add | Sub | Mul | Div

-- | Evaluate an 'Expr' of 'Natural's on the type level.
--
-- Naive, doesn't attempt to tail-call recurse.
type Eval :: Expr Natural -> Natural
type family Eval expr where
    Eval (EBOp bop l r) = EvalBOp bop (Eval l) (Eval r)
    Eval (ELit n) = n

type EvalBOp :: BOp -> Natural -> Natural -> Natural
type family EvalBOp bop l r where
    EvalBOp Add l r = l + r
    EvalBOp Sub l r = l - r
    EvalBOp Mul l r = l * r
    EvalBOp Div l r = l `TypeNats.Div` r

data ExprTok
  = TokBOp BOp
  | TokParenL
  | TokParenR

type PExpr :: PParser s (Expr Natural)
data PExpr ps
type instance App PExpr ps = PExprNext ps '[] '[] (UnconsState ps)
type PExprNext
    :: PState s
    -> [ExprTok]
    -> [Expr Natural]
    -> (Maybe Char, PState s)
    -> PReply s (Expr Natural)
type family PExprNext psPrev ops exprs mps where
    PExprNext psPrev ops exprs '(Just ch, ps) =
        PExprCh psPrev ps ops exprs ch
    PExprNext psPrev ops exprs '(Nothing, ps) = PExprEnd psPrev ps ops exprs

type family PExprEnd psPrev ps ops exprs where
    PExprEnd psPrev ps (TokBOp op:ops) exprs      = PExprEndPopOp psPrev ps ops exprs op
    -- TODO what about parens
    PExprEnd psPrev ps '[]      (expr:'[]) = 'Reply (OK expr) ps
    PExprEnd psPrev ps '[]      _          =
        'Reply (Err (Error1 "badly formed expression")) psPrev

type family PExprEndPopOp sPrev s ops exprs op where
    PExprEndPopOp sPrev s ops (r:l:exprs) bop =
        PExprEnd sPrev s ops (EBOp bop l r : exprs)
    PExprEndPopOp sPrev s ops exprs bop =
        'Reply (Err (Error1 "badly formed expression")) sPrev

type family PExprCh sPrev s ops exprs ch where
    PExprCh sPrev s ops exprs ' ' = PExprNext s ops exprs (UnconsState s)
    PExprCh sPrev s ops exprs ch  = PExprELit sPrev s ops exprs ch (ParseDigitDecSym @@ ch)

type family PExprELit sPrev s ops exprs ch mDigit where
    PExprELit sPrev s ops exprs _ch (Just digit) =
        PExprELitEnd ops exprs
            (While IsDecDigitSym (NatBase1 10 ParseDigitDecSym digit) @@ s)
    PExprELit sPrev s ops exprs  ch Nothing      =
        PExprEBOp sPrev s ops exprs ch (PExprEBOpOpCh ch)

type family PExprELitEnd ops exprs res where
    PExprELitEnd ops exprs ('Reply (OK  n) s) =
        PExprNext s ops (ELit n : exprs) (UnconsState s)
    PExprELitEnd ops exprs ('Reply (Err e) s) =
        -- The digit parser we're wrapping shouldn't ever fail, due to how
        -- 'While' works, and that we've already handled the 0-length case.
        Impossible

type family PExprEBOp psPrev ps ops exprs ch mbop where
    PExprEBOp psPrev ps ops exprs ch (Just (TokBOp bop)) =
        PExprEBOp' psPrev ps bop (BOpPrec bop) exprs ops
    PExprEBOp psPrev ps ops exprs ch (Just TokParenL) =
        PExprNext psPrev (TokParenL:ops) exprs (UnconsState ps)
    PExprEBOp psPrev ps ops exprs ch (Just TokParenR) =
        PExprParenRStart psPrev ps exprs ops
    PExprEBOp psPrev ps ops exprs ch Nothing =
        'Reply (Err (Error1 "bad expression, expected digit or operator")) psPrev

type family PExprParenRStart psPrev ps exprs ops where
    PExprParenRStart psPrev ps exprs (TokParenL : ops) =
        'Reply (Err (Error1 "invalid bracket syntax (empty brackets, or otherwise bad)")) psPrev
    PExprParenRStart psPrev ps exprs ops =
        PExprParenR psPrev ps exprs ops

type family PExprParenR psPrev ps exprs ops where
    PExprParenR psPrev ps exprs (TokBOp bop : ops) =
        PExprParenRPopBOp psPrev ps bop ops exprs
    PExprParenR psPrev ps exprs (TokParenL : ops) =
        PExprNext psPrev ops exprs (UnconsState ps)
    PExprParenR psPrev ps exprs ops =
        'Reply (Err (Error1 "badly formed expression")) psPrev

type family PExprParenRPopBOp psPrev ps bop ops exprs where
    PExprParenRPopBOp psPrev ps bop ops (r:l:exprs) =
        PExprParenR psPrev ps (EBOp bop l r : exprs) ops
    PExprParenRPopBOp psPrev ps bop ops      exprs  =
        'Reply (Err (Error1 "badly formed expression")) psPrev

type family PExprEBOpOpCh ch where
    PExprEBOpOpCh '+' = Just (TokBOp Add)
    PExprEBOpOpCh '-' = Just (TokBOp Sub)
    PExprEBOpOpCh '*' = Just (TokBOp Mul)
    PExprEBOpOpCh '/' = Just (TokBOp Div)
    PExprEBOpOpCh '(' = Just TokParenL
    PExprEBOpOpCh ')' = Just TokParenR
    PExprEBOpOpCh _   = Nothing

type PExprEBOp'
    :: PState s -> PState s -> BOp -> Natural -> [Expr Natural] -> [ExprTok]
    -> PReply s (Expr Natural)
type family PExprEBOp' psPrev ps op prec exprs ops where
    PExprEBOp' psPrev ps op prec exprs (TokBOp opPrev : ops) =
        IfNatLte prec (BOpPrec opPrev)
            (PExprEBOpPop psPrev ps op prec opPrev ops exprs)
            (PExprNext psPrev (TokBOp op : TokBOp opPrev : ops) exprs (UnconsState ps))
    PExprEBOp' psPrev ps op prec exprs '[]          =
        PExprNext ps '[TokBOp op] exprs (UnconsState ps)

    -- both parens treated same as LTE prec
    -- (how could I better design this?)
    PExprEBOp' psPrev ps op prec exprs (TokParenL : ops) =
        PExprNext psPrev (TokBOp op : TokParenL : ops) exprs (UnconsState ps)
    PExprEBOp' psPrev ps op prec exprs (TokParenR : ops) =
        PExprNext psPrev (TokBOp op : TokParenR : ops) exprs (UnconsState ps)

type family PExprEBOpPop psPrev ps op prec opPrev ops exprs where
    PExprEBOpPop psPrev ps op prec opPrev ops (r:l:exprs) =
        PExprEBOp' psPrev ps op prec (EBOp opPrev l r : exprs) ops
    PExprEBOpPop psPrev ps op prec opPrev ops exprs =
        'Reply (Err (Error1 "badly formed expression")) psPrev

type BOpPrec :: BOp -> Natural
type family BOpPrec bop where
    BOpPrec Add = 2
    BOpPrec Sub = 2
    BOpPrec Mul = 3
    BOpPrec Div = 3

{-
import GHC.TypeError qualified as TE

-- | Build an 'Expr' from a postfix stack (RPN style).
--
-- The stack must be a valid 'Expr'. It will type error if not.
type FromRpn:: [ExprTok a] -> Expr a
type FromRpn toks = FromRpnEnd (FromRpn' '[] toks)

type FromRpn' :: [Expr a] -> [ExprTok a] -> [Expr a]
type family FromRpn' es toks where
    FromRpn' es       (TokLit a   : toks) =
        FromRpn' (ELit a       : es) toks
    FromRpn' (r:l:es) (TokBOp bop : toks) =
        FromRpn' (EBOp bop l r : es) toks
    FromRpn' es       '[]                 = es

type family FromRpnEnd res where
    FromRpnEnd    '[]  = TE.TypeError (TE.Text "bad RPN: empty")
    FromRpnEnd (e:'[]) = e
    FromRpnEnd _       = TE.TypeError (TE.Text "bad RPN: unused operands")
-}
