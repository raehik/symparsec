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

module Data.Type.Symbol.Parser where

import GHC.TypeLits
import DeFun.Core ( type (~>), type (@@), type App )
import Data.Type.Symbol ( type Length )
import Data.Type.Symbol.Natural
import Data.Type.Bool ( type If )
import Data.Type.Equality ( type (==) )

data Result s r = Cont s | Done r | Err ErrorMessage

type CharParser s r = Char -> s -> Result s r
type CharParserEnd s r = s -> Either ErrorMessage r

type Drop :: CharParser Natural ()
type family Drop ch s where
    -- | TODO Sorry. My parser design is wonky. This is the easiest solution.
    Drop _ 1 = 'Done '()
    Drop _ n = 'Cont (n-1)

type DropEnd :: CharParserEnd Natural ()
type family DropEnd s where
    DropEnd 0 = 'Right '()
    DropEnd n = 'Left
      ( 'Text "tried to drop "
        :<>: 'ShowType n :<>: 'Text " chars from empty symbol")

type PDrop :: Natural -> CharParserSym' Natural ()
type PDrop n =
    -- TODO This is what's keeping us from starting state with s instead of
    -- Result s r, which would simplify things. I don't like it. Surely there's
    -- a better way to implement Drop.
    '(DropSym, DropEndSym, If (n == 0) ('Done '()) ('Cont n))

type family RunCharParser p sym where
    RunCharParser '(pCh, pEnd, pInit) sym =
        RunCharParserInit pCh pEnd 0 pInit sym

type family RunCharParserInit pCh pEnd idx res sym where
    RunCharParserInit pCh pEnd idx ('Err  e) sym =
        'Left ( 'Text "parse error at index " :<>: 'ShowType idx :$$: e)
    RunCharParserInit pCh pEnd idx ('Done r) sym = 'Right '(r, sym)
    RunCharParserInit pCh pEnd idx ('Cont s) sym =
        RunCharParser' pCh pEnd idx s (UnconsSymbol sym)

type family RunCharParser' pCh pEnd idx s sym where
    RunCharParser' pCh pEnd idx s 'Nothing =
        RunCharParser'End idx (pEnd @@ s)
    RunCharParser' pCh pEnd idx s ('Just '(ch, sym)) =
        RunCharParser'' pCh pEnd idx ch (pCh @@ ch @@ s) sym

type family RunCharParser'' pCh pEnd idx prevCh res sym where
    RunCharParser'' pCh pEnd idx prevCh ('Err  e) sym =
        'Left ( 'Text "parse error at index " :<>: 'ShowType idx
                :<>: 'Text " char " :<>: 'ShowType prevCh :$$: e)
    RunCharParser'' pCh pEnd idx prevCh ('Done r) sym =
        'Right '(r, sym)
    RunCharParser'' pCh pEnd idx prevCh ('Cont s) sym =
        RunCharParser' pCh pEnd (idx+1) s (UnconsSymbol sym)

type family RunCharParser'End idx res where
    RunCharParser'End idx ('Left  e) = 'Left e
    RunCharParser'End idx ('Right r) = 'Right '(r, "")

type CharParserSym s r = Char ~> s ~> Result s r
type CharParserEndSym s r = s ~> Either ErrorMessage r

type CharParserSym' s r = (CharParserSym s r, CharParserEndSym s r, Result s r)

type DropSym :: CharParserSym Natural ()
data DropSym f
type instance App DropSym f = DropSym1 f

type DropSym1 :: Char -> Natural ~> Result Natural ()
data DropSym1 ch n
type instance App (DropSym1 ch) n = Drop ch n

type DropEndSym :: CharParserEndSym Natural ()
data DropEndSym n
type instance App DropEndSym n = DropEnd n

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

{-
type family RunCharParser'' pCh pEnd idx s sym where
    RunCharParser'' pCh pEnd idx ('Err  e) _ = 'Left  e
    RunCharParser'' pCh pEnd idx ('Done r) 'Nothing =
        -- TODO early Done keeps prev char (fixes Drop)
        'Right '(r, "")
    RunCharParser'' pCh pEnd idx ('Done r) ('Just '(ch, sym)) =
        -- TODO early Done keeps prev char (fixes Drop)
        'Right '(r, ConsSymbol prevCh (ConsSymbol ch sym))
    RunCharParser'' pCh pEnd idx prevCh ('Cont s) 'Nothing =
        RunCharParser'End (pEnd @@ s)
    RunCharParser'' pCh pEnd idx prevCh ('Cont s) ('Just '(ch, sym)) =
        RunCharParser'' pCh pEnd idx prevCh (pCh @@ ch @@ s) (UnconsSymbol sym)
-}

{-
type PState = (Natural, Char)
type Parser a =
    PState -> Maybe (Char, Symbol) -> Either ErrorMessage (PState, a)
type ParserSym e a =
    PState ~> Maybe (Char, Symbol) ~> Either ErrorMessage (PState, a)

type ParserF e a = (Maybe Char ~> a ~> Result e a r) -> (r, Symbol)
type DropFParse :: ParserF Natural ()
type DropFEnd :: ParserF () Natural Symbol
-}
