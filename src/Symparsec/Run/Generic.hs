{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Run.Generic
{-
  ( type ValidateField
  , validateFields
  ) where
-}
    where

import Symparsec.Parser
import Symparsec.Run ( type Run' )
--import GHC.TypeLits
import Data.Kind ( type Constraint, type Type)
import GHC.Exts ( type Any )
import GHC.TypeError qualified as TE
import GHC.TypeError
import GHC.TypeLits
import GHC.Generics
import Raehik.Experiment.GenericK

{- 2026-06-11T09:16:55+0100
Wait, TODO. There's no reason to squish this inside other generics, since we
aren't actually parsing anything. Validation should be its own function (it can
actually stay almost entirely in type-level).

So... where to evaluate this, then? Do I need to ask the user to create a fresh
type class? Can I provide one, and have them fill in instances? Can I just make
one, and have people write a top-level definition? (But then it's unused, which
can be an issue.)
-}

-- This is a good start, but I still require the user to ferry these to me,
-- which means multiple generic typeclasses (unlike many tutorials!).
-- What could I do for users who want to write even simpler generics?
-- Can I simply omit the datatype name & hope that GHC locates the error well?
type ValidateField
  :: Symbol -> Symbol -> Maybe Symbol -> PParser () a -> Constraint
type family ValidateField dtName cstrName mFieldName p where
  ValidateField dtName cstrName Nothing          p =
    -- If one field is unnamed, then all fields in that constructor are,
    -- so emit an appropriate error message.
    TypeError
      (      Text "Constructor does not use record syntax: "
        :<>: Text dtName :<>: Text "." :<>: Text cstrName
        :$$: Text "This function may only be used on constructors with named fields." )
  ValidateField dtName cstrName (Just fieldName) p = ValidateField'
    (Text dtName :<>: Text "." :<>: Text cstrName :<>: Text "." :<>: Text fieldName)
    (Run' p fieldName)

-- fieldIdent should identify the field as precisely as possible:
-- ideally @dtName.cstrName.fieldName@, otherwise at least fieldName
type ValidateField' :: TE.ErrorMessage -> Either TE.ErrorMessage a -> Constraint
type family ValidateField' fieldIdent res where
  -- ignore remaining unparsed
  ValidateField' fieldIdent (Right _) = ()
  ValidateField' fieldIdent (Left  e) = TypeError
    ( Text "Field name verification failed in: " :<>: fieldIdent :$$: e )

--class X (parser :: PParser () Symbol) a where xxx :: a x -> ()

type VValidateFields :: PParser () pa -> a x -> Constraint
type family VValidateFields parser a where
  VValidateFields parser (D1 (MetaData dtName _ _ _) a) =
    VValidateFieldsD parser dtName a

type VValidateFieldsD :: PParser () pa -> Symbol -> a x -> Constraint
type family VValidateFieldsD parser dtName a where
  VValidateFieldsD parser dtName (C1 (MetaCons cstrName _ _) U1) = ()
  VValidateFieldsD parser dtName (C1 (MetaCons cstrName _ _) a) =
    VValidateFieldsC parser dtName cstrName '[a]
  -- TODO: permit cstr sum

{-
-- TODO is this slow? I could instead put the contents of C1 into a list, and
-- use that as a stack.
type VValidateFieldsC :: PParser () pa -> a x -> Constraint
type family VValidateFieldsC parser a where
  VValidateFieldsCs parser dtName cstrName (l :*: r)=
    VValidateFieldsC parser dtName a
-}

-- Improvement: Check the first field of a given cstr.
-- If it's record syntax, then all fields are, else all fields aren't.
-- (Then we can omit checking without running the risk of getting stuck.)
type VValidateFieldsC :: PParser () pa -> Symbol -> Symbol -> [a x] -> Constraint
type family VValidateFieldsC parser dtName cstrName as where
  VValidateFieldsC parser dtName cstrName ((l:*:r) : as) =
    VValidateFieldsC parser dtName cstrName (l : r : as)
  VValidateFieldsC parser dtName cstrName (S1 (MetaSel (Just fieldName) _ _ _) _a : as) =
    VValidateFieldsF parser dtName cstrName fieldName as (Run' parser fieldName)
  VValidateFieldsC parser dtName cstrName '[] = ()
  VValidateFieldsC parser dtName cstrName (S1 (MetaSel Nothing _ _ _) : _) =
    TypeError (Text "TODO a cstr wasn't record syntax")

type VValidateFieldsF :: PParser () pa -> Symbol -> Symbol -> Symbol -> [a x] -> Either TE.ErrorMessage (pa, Symbol) -> Constraint
type family VValidateFieldsF parser dtName cstrName fieldName as res where
  -- ignore remaining unparsed
  VValidateFieldsF parser dtName cstrName fieldName as (Right _) =
    VValidateFieldsC parser dtName cstrName as
  VValidateFieldsF parser dtName cstrName fieldName as (Left  e) = TypeError
    (      Text "Field name verification failed in: "
      :<>: Text dtName :<>: Text "." :<>: Text cstrName :<>: Text "." :<>: Text fieldName
      :$$: e )

-- Great! But the second RequiredTypeArg is likely awkward, you may have to
-- use (type X) to select the right thing. Minor ergonomics.
validateFields
  :: forall (parser :: PParser () pa)
  -> forall a
  -> (Generic a, VValidateFields parser (Rep a))
  => ()
validateFields _parser _a = ()

{- Something interesting to consider:
Data type metadata is separate from parametricity/type args.
It'd be nice if we could pass any @k -> Type@ (?) and fill in @k@ with anything,
since we don't use it.

Right, I'd probably need something like GenericK from kind-generics,
which is very different.

Maybe I can do some magic without kind-generics, where I take some @ty :: k@ and
match on the kind like typelits-printf until I get to @Type@.
For each tyvar, I should be able to use @Any@, right?
-}

class VFGK (cls :: (Type -> Type) -> Constraint) (a :: k)
instance VFGK cls (a Any) => VFGK cls (a :: x -> k)
instance cls (Rep a) => VFGK cls (a :: Type)

-- I need this because VValidateFieldsD is a type synonym, and I can't partially
-- apply it.
class Asdf (parser :: PParser () pa) (rep :: Type -> Type)
instance VValidateFieldsD parser dtName rep => Asdf parser (D1 (MetaData dtName _x1 _x2 _x3) rep)

data XYZ a = XYZ1 deriving stock Generic
vfgk :: forall cls -> forall a -> VFGK cls a => ()
vfgk _cls _a = ()

-- You might have to quote k with @type@ if your constructor & type names equal.
-- (RequiredTypeArguments has different implicit assumptions regarding punning.)
validateGeneric
    :: forall cls
    -> forall k
    -> (GenericK k, cls (RepK k))
    => ()
validateGeneric _cls _k = ()

-- OK, so this kinda works LMAO
