{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Raehik.Experiment.GenericK
  ( module Raehik.Experiment.GenericK
  , Any
  -- ^ type errors look better (not fully-qualified) if we do this
  ) where

import Symparsec.Parser
import Symparsec.Run ( type Run' )
--import GHC.TypeLits
import Data.Kind ( type Constraint, type Type)
import GHC.Exts ( type Any )
import GHC.TypeError qualified as TE
import GHC.TypeError
import GHC.TypeLits
import GHC.Generics

-- | Saturate some type constructor @a :: k@ with unmatchable types 'Any'.
--
-- When fully applied, @a :: k@ must be a 'Type'.
-- For example, @'SatTyCon' 'Maybe' = 'Maybe' 'Any' :: 'Type'@,
--          but @'SatTyCon' 'Just'  = 'Just'  'Any' :: 'Maybe' a@,
-- so emits a type error. i.e. Prevents saturating promoted data constructors.
--
-- If you call this on a promoted data constructor, it will get stuck.
-- This is because if we add a type error equation in here, GHC will report it,
-- but I'm already reporting type errors in a type class instead (for better
-- ergonomics). All this should be irrelevant to the end user, because this type
-- family is not exported.
type SatTyCon :: k -> Type
type family SatTyCon a where
  SatTyCon (f :: x -> k) = SatTyCon (f Any)
  SatTyCon (a :: Type)   = a

type RepK k = Rep (SatTyCon k)

-- | Saturates a type constructor with unuseable 'Any's and constrains on
--   'Generic'.
--
-- Potentially useful for performing "structural" generics, which only permit
-- operating on the ADT metadata, not the actual types stored. In such cases,
-- using this class means you don't need to fully apply your type to use it.
--
-- e.g. @'GenericK' 'Either' ~ 'Generic' ('Either' 'Any' 'Any')@
--
-- This type class permits us better type errors, as 'RepK' cannot be defined as
-- an associated type synonym (due to needing overlapping equations).
-- Constraining on 'GenericK' adds the appropriate 'Generic' constraint, which
-- in turn implies 'RepK' will not get stuck.
--
-- You can still attempt to use 'RepK' without constraining on 'GenericK'.
-- But by adding this constraint, GHC will emit clearer type errors:
--
-- - If @x :: x -> k@, we instantiate all type variables with 'Any'.
-- - If @x :: Type@, it's simply @'Generic' a@. (If this instance doesn't exist,
--   GHC will emit its regular type error.)
-- - If @x :: k@ where @k@ is fully applied, it's a non-Type e.g. promoted data
--   type, which does not permit a 'Generic' instance. In this case, we emit a
--   custom type error suggesting the likely reason (punning/namespace rules)
--   and solution (disambiguate using @type@).
class GenericK x
instance GenericK (f Any) => GenericK (f :: x -> k)
instance Generic a => GenericK (a :: Type)
instance {-# OVERLAPS #-} Unsatisfiable (ErrNonTypeGeneric a) => GenericK (a :: k)

type ErrNonTypeGeneric (a :: k) =
       Text "Non-Type kinds do not permit a Generic instance:"
  :$$: ShowType a :<>: Text " :: " :<>: ShowType k
  :$$: Text "If you are using RequiredTypeArguments, you may need to disambiguate using the ‘type’ keyword"
  :$$: Text "i.e. ‘f Person’ -> ‘f (type Person)’ (requires ExplicitNamespaces)"

type GenericK' k = Generic (SatTyCon k)
-- This is fine, but errors aren't good, because the RHS reduces to
-- @Generic (SatTyCon (...))@. Should I implement this as a typeclass?
-- Isn't this related to Csongor's blog post on stuck type families?

{-

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

-- OK, so this kinda works LMAO

-}
