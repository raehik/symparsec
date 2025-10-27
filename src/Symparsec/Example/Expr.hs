{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE MultilineStrings #-} -- GHC 9.12, for TestProg

module Symparsec.Example.Expr where

import Symparsec.Parser.Common
import Symparsec.Utils ( type IfNatLte )
import Symparsec.Parsers
import Symparsec.Parser.Natural.Digits
import Symparsec.Parser.While.Predicates ( type IsDecDigitSym, type IsAlphaSym, type IsChar )
--import GHC.TypeNats qualified as TypeNats
import DeFun.Core

-- SAKS necessitates TypeAbstractions
type VarParser :: PParser s Symbol
type VarParser @s = TakeWhile1 @s IsAlphaSym

-- | A binary operator.
data BOp = Add | Sub | Mul | Div

data Expr str a
  = EBOp BOp (Expr str a) (Expr str a)
  | ELit a
  | EVar str
--  | ELet str (Expr str a) (Expr str a)
type PExpr = Expr Symbol Natural

data ExprTok
  = TokBOp BOp
  | TokParenL
  | TokParenR

type ExprParser :: PParser s PExpr
data ExprParser ps
type instance App ExprParser ps = PExprNext ps '[] '[] (UnconsState ps)
type PExprNext
    :: PState s
    -> [ExprTok]
    -> [PExpr]
    -> (Maybe Char, PState s)
    -> PReply s PExpr
type family PExprNext psPrev ops exprs mps where
    PExprNext psPrev ops exprs '(Just ch, ps) =
        PExprCh psPrev ps ops exprs ch
    PExprNext psPrev ops exprs '(Nothing, ps) = PExprEnd psPrev ps ops exprs

type family PExprEnd psPrev ps ops exprs where
    PExprEnd psPrev ps (TokBOp op:ops) exprs = PExprEndPopOp psPrev ps ops exprs op
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

type family PExprELit psPrev ps ops exprs ch mDigit where
    PExprELit psPrev ps ops exprs _ch (Just digit) =
        PExprELitEnd ops exprs
            (While IsDecDigitSym (NatBase1 10 ParseDigitDecSym digit) @@ ps)
    PExprELit psPrev ps ops exprs  ch Nothing      =
        PExprEVarEnd ps ch ops exprs (VarParser @@ psPrev)

type family PExprELitEnd ops exprs res where
    PExprELitEnd ops exprs ('Reply (OK  n) ps) =
        PExprNext ps ops (ELit n : exprs) (UnconsState ps)
    PExprELitEnd ops exprs ('Reply (Err e) ps) =
        -- The digit parser we're wrapping shouldn't ever fail, due to how
        -- 'While' works, and that we've already handled the 0-length case.
        Impossible

-- TODO weird state parsing here. usually we're looking ahead by 1 char, but my
-- VarParser is better and doesn't need to. BUT, we need to keep our lookahead
-- state for the BOp parser. lol
type family PExprEVarEnd ps ch ops exprs rep where
    PExprEVarEnd ps ch ops exprs ('Reply (OK  v) ps') =
        PExprNext ps' ops (EVar v : exprs) (UnconsState ps')
    PExprEVarEnd ps ch ops exprs ('Reply (Err e) psPrev) =
        PExprEBOp psPrev ps ops exprs ch (PExprEBOpOpCh ch)

type family PExprEBOp psPrev ps ops exprs ch mbop where
    PExprEBOp psPrev ps ops exprs ch (Just (TokBOp bop)) =
        PExprEBOp' psPrev ps bop (BOpPrec bop) exprs ops
    PExprEBOp psPrev ps ops exprs ch (Just TokParenL) =
        PExprNext psPrev (TokParenL:ops) exprs (UnconsState ps)
    PExprEBOp psPrev ps ops exprs ch (Just TokParenR) =
        PExprParenRStart psPrev ps exprs ops
    PExprEBOp psPrev ps ops exprs ch Nothing =
        -- TODO erroring here means PExpr must consume WHOLE string lol.
        -- 'Reply (Err (Error1 "bad expression, expected digit or operator")) psPrev
        PExprEnd psPrev psPrev ops exprs

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
    :: PState s -> PState s -> BOp -> Natural -> [PExpr] -> [ExprTok]
    -> PReply s PExpr
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
-}

data Decl str a = Decl
  { name :: str
  , expr :: Expr str a
  }
type PDecl = Decl Symbol Natural

type DeclParser :: PParser s PDecl
type DeclParser @s = LiftA2 @s (Con2 'Decl) (VarParser @s <* TakeWhile (IsChar ' ')) (Literal @s ":=" *> ExprParser @s)

type DeclListParser :: PParser s [PDecl]
type DeclListParser @s = SepBy DeclParser (TakeWhile1 (IsChar '\n')) <* Eof

type TestProg = """
abc := 1+2+3
xyz := abc / 2
"""
