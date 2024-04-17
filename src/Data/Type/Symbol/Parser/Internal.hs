{-# LANGUAGE UndecidableInstances #-}

{-
Oh no! ToNat needs its state to be initialized using the input symbol's length!
I'm not sure it will work as a CharParser now. Unless parsers _also_ provide a
@Symbol -> s@ state initialization function, which takes the whole symbol :/

Ack, but state initialization was kind of relegated to the user, and it's used
to fill out parameters for parameterized parsers (number of chars to drop, digit
parsers to use, etc.).

Why bother? Especially with something like Drop, which seems exceedingly simple?
Well, we can only inspect type-level symbols by unconsing characters. So Drop
has to be implemented in a similar method anyway.

TODO 2024-04-17T03:28:26+0100

  * my main runner appears to be eating one too many characters. Drop has had
    this problem for a while, but Isolate appears to experience this also.
  * probably want to clean up state/return passing to make it a little easier to
    use. not too sure exactly how it'll look, but don't want to have to write a
    Void type to ignore empty return types.
-}

module Data.Type.Symbol.Parser.Internal where

import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@) )

data Result s r = Cont s | Done r | Err ErrorMessage

type Parser s r = Char -> s -> Result s r
type ParserEnd s r = s -> Either ErrorMessage r

type ParserSym s r = Char ~> s ~> Result s r
type ParserEndSym s r = s ~> Either ErrorMessage r

type ParserSym' s r = (ParserSym s r, ParserEndSym s r, s)

type family RunParser p sym where
    RunParser '(pCh, pEnd, s) sym =
        RunParser' pCh pEnd 0 s (UnconsSymbol sym)

-- TODO maybe take an mch? Nothing at start, Just otherwise
type family RunParser' pCh pEnd idx s msym where
    RunParser' pCh pEnd idx s 'Nothing =
        RunParserEnd idx (pEnd @@ s)
    RunParser' pCh pEnd idx s ('Just '(ch, sym)) =
        RunParser'' pCh pEnd idx ch (pCh @@ ch @@ s) sym

type family RunParserEnd idx end where
    RunParserEnd idx ('Left  e) = 'Left e
    RunParserEnd idx ('Right r) = 'Right '(r, "")

type family RunParser'' pCh pEnd idx ch res sym where
    RunParser'' pCh pEnd idx ch ('Err  e) sym = 'Left e -- TODO annotate error
    RunParser'' pCh pEnd idx ch ('Done r) sym = 'Right '(r, sym)
    RunParser'' pCh pEnd idx ch ('Cont s) sym =
        RunParser' pCh pEnd (idx+1) s (UnconsSymbol sym)

-- TODO could do this if more parsers end up storing state which they emit
-- precisely (NatBase does this)
--type ParserEndEmit :: ParserEnd r r

{-

-- TODO not using pInit. clumsy
type Isolate :: CharParserSym' s r -> CharParser (Natural, s) r
type family Isolate p ch s where
    Isolate '(pCh, pEnd, pInit) ch '(0, s) = IsolateWrapEnd  (pEnd @@ s)
    Isolate '(pCh, pEnd, pInit) ch '(n, s) = IsolateWrapCont n (pCh @@ ch @@ s)

type IsolateSym
    :: CharParserSym' s r
    -> CharParserSym (Natural, s) r
data IsolateSym p f
type instance App (IsolateSym p) f = IsolateSym1 p f

type IsolateSym1
    :: CharParserSym' s r
    -> Char -> (Natural, s) ~> Result (Natural, s) r
data IsolateSym1 p ch s
type instance App (IsolateSym1 p ch) s = Isolate p ch s

type IsolateEnd :: CharParserEnd (Natural, s) r
type family IsolateEnd s where
    IsolateEnd '(0, s) = 'Right '(0, s)
    IsolateEnd '(n, s) =
        -- TODO
        'Left ('Text "isolate wanted more than was there")

-- TODO have to pass init state here too awkwardly :/
type PIsolate
    :: Natural -> CharParserSym' s r -> s -> CharParserSym' (Natural, s) r
type PIsolate n p s =
    '(IsolateSym p, IsolateEndSym, 'Cont '(n, s))

type IsolateEndSym :: CharParserEndSym (Natural, s) r
data IsolateEndSym s
type instance App IsolateEndSym s = IsolateEnd s

type IsolateWrapEnd :: Either ErrorMessage r -> Result (Natural, s) r
type family IsolateWrapEnd a where
    IsolateWrapEnd ('Left  e) = 'Err  e
    IsolateWrapEnd ('Right r) = 'Done r

type IsolateWrapCont :: Natural -> Result s r -> Result (Natural, s) r
type family IsolateWrapCont n a where
    IsolateWrapCont _ ('Err  e) = 'Err  e
    IsolateWrapCont _ ('Done _) =
        -- TODO put n in that error too plz
        'Err ('Text "isolated parser ended without consuming all input")
    IsolateWrapCont n ('Cont s) = 'Cont '(n-1, s)

type PToNat
    :: Natural -> (Char ~> Maybe Natural) -> CharParserSym' Natural Natural
type PToNat base parseDigit =
    '(ToNatSym base parseDigit, ToNatEndSym, 'Cont 0)

type ToNat
    :: Natural
    -> (Char ~> Maybe Natural)
    -> CharParser Natural Natural
type family ToNat base parseDigit ch n where
    ToNat base parseDigit ch n =
        ToNat' base n (parseDigit @@ ch)

type family ToNat' base n mDigit where
    ToNat' base n 'Nothing      =
        'Err ('Text "not a base " :<>: 'ShowType base :<>: 'Text " digit")
    ToNat' base n ('Just digit) = 'Cont (n * base + digit)

type ToNatEnd :: CharParserEnd Natural Natural
type family ToNatEnd s where
    ToNatEnd n = 'Right n

type ToNatSym
    :: Natural
    -> (Char ~> Maybe Natural)
    -> CharParserSym Natural Natural
data ToNatSym base parseDigit f
type instance App (ToNatSym base parseDigit) f = ToNatSym1 base parseDigit f

type ToNatSym1
    :: Natural
    -> (Char ~> Maybe Natural)
    -> Char -> Natural
    ~> Result Natural Natural
data ToNatSym1 base parseDigit ch n
type instance App (ToNatSym1 base parseDigit ch) n = ToNat base parseDigit ch n

type ToNatEndSym :: CharParserEndSym Natural Natural
data ToNatEndSym n
type instance App ToNatEndSym s = ToNatEnd s

-}
