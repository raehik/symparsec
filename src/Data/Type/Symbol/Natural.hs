{-# LANGUAGE UndecidableInstances #-}

{- | Parse 'Natural's from type-level 'Symbol's.

The type functions here may return errors. Use the provided and throw with
'TypeError', or wrap in your own error handling.

TODO

  * Oh dear. Oh dear. Oh dear. If I want to get proper working composition with
    meaningful errors, where the index pays attention to what we Drop... then I
    need to write a type-level parser monad. That's it. These parsers need to
    take in parser state (well, just character index is OK), and emit that state
    on success. Oh dear. Oh no.
-}

module Data.Type.Symbol.Natural where

import Data.Type.Char.Digits
import GHC.TypeLits
import Data.Type.Bool ( type If )
import Data.Type.Equality ( type (==) )
import DeFun.Core ( type (~>), type App, type (@@) )
import Data.Type.Symbol ( type Length )

-- | Parse a 'Symbol' describing a  binary     (base  2) natural
--   to its 'Natural' value.
type ParseBinarySymbol  sym = ParseSymbolDigits  2 ParseBinaryDigitSym  sym

-- | Parse a 'Symbol' describing an octal      (base  8) natural
--   to its 'Natural' value.
type ParseOctalSymbol   sym = ParseSymbolDigits  8 ParseOctalDigitSym   sym

-- | Parse a 'Symbol' describing a  decimal    (base 10) natural
--   to its 'Natural' value.
type ParseDecimalSymbol sym = ParseSymbolDigits 10 ParseDecimalDigitSym sym

-- | Parse a 'Symbol' describing a hexadecimal (base 16) natural
--   to its 'Natural' value.
type ParseHexSymbol     sym = ParseSymbolDigits 16 ParseHexDigitSym     sym

type family PrettyE e where
    PrettyE 'EEmptySymbol = 'Text "empty symbol"
    PrettyE ('EBadDigit base ch idx) = PrettyEBadDigit base ch idx

type family MapLeftPrettyE e where
    MapLeftPrettyE ('Right a) = 'Right a
    MapLeftPrettyE ('Left  e) = 'Left (PrettyE e)

type family FromRightParseResult sym eab where
    FromRightParseResult _   ('Right a) = a
    FromRightParseResult sym ('Left  e) = TypeError
        (      'Text "error while parsing symbol: " :<>: 'Text sym
          :$$: PrettyE e
        )

data E = EBadDigit Natural Char Natural | EEmptySymbol

type PrettyEBadDigit base ch idx =
         'Text "could not parse character as base "
    :<>: 'ShowType base :<>: 'Text " digit"
    :$$: 'ShowType ch :<>: 'Text " at index " :<>: 'ShowType idx

-- | Parse a symbol to a 'Natural' using the given base and digit parser.
type ParseSymbolDigits base tfDigitValue sym =
    If (Length sym == 0) ('Left 'EEmptySymbol)
        (WrapEBadDigit base
            (ParseSymbolDigits' base tfDigitValue ('Just 0) '\0' 0 (Length sym - 1) (UnconsSymbol sym)))

type family WrapEBadDigit base eab where
    WrapEBadDigit _    ('Right b) = 'Right b
    WrapEBadDigit base ('Left  '(ch, sym)) = 'Left ('EBadDigit base ch sym)

type ParseSymbolDigits'
    :: Natural                 {- ^ base -}
    -> (Char ~> Maybe Natural) {- ^ digit parser (defun symbol) -}
    -> Maybe Natural           {- ^ accumulator (Nothing means failure) -}
    -> Char                    {- ^ previous parsed character -}
    -> Natural                 {- ^ index in symbol -}
    -> Natural                 {- ^ current exponent -}
    -> Maybe (Char, Symbol)    {- ^ remaining symbol -}
    -> Either (Char, Natural) Natural
type family ParseSymbolDigits' base tfParseDigit mn prevCh idx expo mchsym where
    ParseSymbolDigits' base tfParseDigit ('Just n) prevCh idx expo 'Nothing =
        -- previous digit parsed, no more characters: all done
        'Right n
    -- note that the above will return 0 for empty symbols!
    -- we could add an equation for that... but it would be inefficient, since
    -- we only need to check once. instead, we handle that outside.
    ParseSymbolDigits' base tfParseDigit 'Nothing  prevCh idx expo mchsym =
        -- digit parse error: emit problematic 'Char' and its index
        -- the -1 is clumsy but the easiest way to achieve zero-indexing
        -- safe: we always start with 'Just, so if we get here we're at least 1
        'Left '(prevCh, idx-1)
    ParseSymbolDigits' base tfParseDigit ('Just n) prevCh idx expo ('Just '(ch, sym)) =
        -- previous digit parsed, characters remaining: parse next digit
        ParseSymbolDigits' base tfParseDigit
            (ParseSymbolDigits'Inc (base^expo) n (tfParseDigit @@ ch))
            ch (idx+1) (expo-1) (UnconsSymbol sym)

-- little helper for incrementing accumulator, or failing to 'Nothing'
type family ParseSymbolDigits'Inc mult n mDigit where
    ParseSymbolDigits'Inc mult n 'Nothing      = 'Nothing
    ParseSymbolDigits'Inc mult n ('Just digit) = 'Just (n + digit*mult)

type ParseBinaryDigitSym :: Char ~> Maybe Natural
data ParseBinaryDigitSym a
type instance App ParseBinaryDigitSym a = ParseBinaryDigit a

type ParseOctalDigitSym :: Char ~> Maybe Natural
data ParseOctalDigitSym a
type instance App ParseOctalDigitSym a = ParseOctalDigit a

type ParseDecimalDigitSym :: Char ~> Maybe Natural
data ParseDecimalDigitSym a
type instance App ParseDecimalDigitSym a = ParseDecimalDigit a

type ParseHexDigitSym :: Char ~> Maybe Natural
data ParseHexDigitSym a
type instance App ParseHexDigitSym a = ParseHexDigit a
