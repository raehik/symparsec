{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Example.Expr where

import Symparsec.Parser.Common
import Symparsec.Utils ( type IfNatLte )
import Symparsec.Parser.Natural
import Symparsec.Parser.Natural.Digits
import Symparsec.Parser.While ( type While )
import Symparsec.Parser.While.Predicates ( type IsDecDigitSym )

type PExpr :: PParser (Expr Natural)
data PExpr s
type instance App PExpr s = PExprNext '[] '[] (UnconsState s)
type PExprNext
    :: [BOp]
    -> [Expr Natural]
    -> (Maybe Char, PState)
    -> PReply (Expr Natural)
type family PExprNext ops exprs s where
    PExprNext ops exprs '(Just ch, s) =
        PExprCh s ops exprs ch
    PExprNext ops exprs '(Nothing, s) = PExprEnd s ops exprs

type family PExprEnd s ops exprs where
    PExprEnd s (op:ops) exprs      = PExprEndPopOp s ops exprs op
    PExprEnd s '[]      (expr:'[]) = 'Reply (OK expr) s
    PExprEnd s '[]      _          =
        'Reply (Err (Error1 "badly formed expression")) s

type family PExprEndPopOp s ops exprs op where
    PExprEndPopOp s ops (r:l:exprs) bop =
        PExprEnd s ops (EBOp bop l r : exprs)

type family PExprCh s ops exprs ch where
    PExprCh s ops exprs ' ' = PExprNext ops exprs (UnconsState s)
    PExprCh s ops exprs ch  = PExprELit s ops exprs ch (ParseDigitDecSym @@ ch)

type family PExprELit s ops exprs ch mDigit where
    PExprELit s ops exprs _ch (Just digit) =
        PExprELitEnd ops exprs
            (While IsDecDigitSym (NatBase1 10 ParseDigitDecSym digit) @@ s)
    PExprELit s ops exprs  ch Nothing      =
        PExprEBOp s ops exprs ch

type family PExprELitEnd ops exprs res where
    PExprELitEnd ops exprs ('Reply (OK  n) s) =
        PExprNext ops (ELit n : exprs) (UnconsState s)
    PExprELitEnd ops exprs ('Reply (Err e) s) =
        'Reply (Err e) s

type family PExprEBOp s ops exprs ch where
    PExprEBOp s ops exprs '+' =
        PExprEBOp' s Plus (BOpPrec Plus) exprs ops
    PExprEBOp s ops exprs '*' =
        PExprEBOp' s Mult (BOpPrec Mult) exprs ops

type family PExprEBOp' s op prec exprs ops where
    PExprEBOp' s op prec exprs (opPrev:ops) =
        IfNatLte prec (BOpPrec opPrev)
            (PExprEBOpPop s op prec opPrev ops exprs)
            (PExprNext (op:opPrev:ops) exprs (UnconsState s))
    PExprEBOp' s op prec exprs '[]          =
        PExprNext '[op] exprs (UnconsState s)

type family PExprEBOpPop s op prec opPrev ops exprs where
    PExprEBOpPop s op prec opPrev ops (r:l:exprs) =
        PExprEBOp' s op prec (EBOp opPrev l r : exprs) ops
    -- TODO add errorful case

type BOpPrec :: BOp -> Natural
type family BOpPrec bop where
    BOpPrec Plus = 2
    BOpPrec Mult = 3

data Expr a
  = EBOp BOp (Expr a) (Expr a)
  | ELit a

data BOp
  = Plus
  | Mult

-- | 'Expr' tokens.
data ExprTok a
  = TokBOp BOp
  | TokLit a

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
