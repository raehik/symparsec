{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Token ( type Token ) where

import Symparsec.Parser.Common

-- | Should match @token@ from megaparsec. Backtracks.
type Token :: (Char ~> Maybe a) -> PParser s a
data Token chParse ps
type instance App (Token chParse) ps = TokenStart chParse ps (UnconsState ps)
type family TokenStart chParse psPrev mps where
    TokenStart chParse psPrev '(Just ch, ps) = TokenParse psPrev ps (chParse @@ ch)
    TokenStart chParse psPrev '(Nothing, ps) =
        'Reply (Err (Error1 "expected at least 1 char")) ps
type family TokenParse psPrev ps res where
    TokenParse psPrev ps (Just a) = 'Reply (OK a) ps
    TokenParse psPrev ps Nothing  =
        'Reply (Err (Error1 "token: char failed parse (TODO print char here)")) psPrev
