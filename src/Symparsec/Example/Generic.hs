{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Example.Generic where

import GHC.Generics
import Symparsec.Parsers
import Symparsec qualified
import Symparsec.Parser qualified as Symparsec
import Symparsec.Run.Generic qualified as Symparsec
import GHC.TypeLits
import Data.Proxy

-- | A product type with field names prefixed with the type name (often done to
--   avoid clobbering existing definitions).
data Person = Person
  { personName :: String
  , personAge :: Natural
  } deriving stock Generic

-- | A serializer from 'a' to 'String'.
newtype ToString a = ToString { unToString :: a -> String }

genericToString
    :: forall (parser :: Symparsec.PParser () Symbol)
    -> (Generic a, GToString parser (Rep a))
    => a -> String
genericToString parser = gToString @parser . from

class GToString (parser :: Symparsec.PParser () Symbol) a where gToString :: a x -> String
instance GToString parser a => GToString parser (D1 _x1 a) where
    gToString = gToString @parser . unM1
-- omit (:+:) instance, since uninterested in sum types
instance GToString parser a => GToString parser (C1 _x1 a) where
    gToString = gToString @parser . unM1
instance (GToString parser l, GToString parser r) => GToString parser (l :*: r) where
    gToString (l :*: r) = gToString @parser l <> "\n" <> gToString @parser r
-- bad error handling, but one instance
instance (Symparsec.Run' parser recNm ~ Right '(recNm', ""), KnownSymbol recNm', Show a) => GToString parser (S1 (MetaSel (Just recNm) _x1 _x2 _x3) (Rec0 a)) where
    gToString (M1 (K1 a)) = symbolVal (Proxy @recNm') <> ": " <> show a
-- omit instance where field name is not provided

type PersonParser = Literal "person" *> TakeRest

data Struct = Struct
  { struct00 :: String
  , struct10 :: Natural
  , structFF :: Natural
  } deriving stock Generic

-- generic toString on non-sum type, where we validate field names
genericToStringValidate
    :: forall (parser :: Symparsec.PParser () pa)
    -> (Generic a, GToStringValidate parser (Rep a))
    => a -> String
genericToStringValidate parser = gToStringValidate @_ @parser . from

class GToStringValidate (parser :: Symparsec.PParser () pa) a where
    gToStringValidate :: a x -> String
instance GToStringValidate parser a => GToStringValidate parser (D1 _x1 a) where
    gToStringValidate = gToStringValidate @_ @parser . unM1
-- omit (:+:) instance, since uninterested in sum types
instance GToStringValidate parser a => GToStringValidate parser (C1 _x1 a) where
    gToStringValidate = gToStringValidate @_ @parser . unM1
instance (GToStringValidate parser l, GToStringValidate parser r) => GToStringValidate parser (l :*: r) where
    gToStringValidate (l :*: r) = gToStringValidate @_ @parser l <> "\n" <> gToStringValidate @_ @parser r
-- bad error handling, but one instance
instance (Symparsec.ValidateField "TODO" "TODO" mFieldName parser, Show a)
  => GToStringValidate parser (S1 (MetaSel mFieldName _x1 _x2 _x3) (Rec0 a)) where
    gToStringValidate (M1 (K1 a)) = "TODO: " <> show a
