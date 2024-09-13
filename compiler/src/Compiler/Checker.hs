module Compiler.Checker where

import qualified Compiler.AST as AST
import Compiler.Type
import Control.Applicative (Applicative(liftA2))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Function (on)
import Control.Monad (join, liftM2)
import Data.Maybe (isNothing)

inferType :: AST.Expression -> Type
inferType = undefined

intersectType :: Type -> Type -> Type
intersectType a b = Type {
  typeNominal = TypeNominal Set.empty,
  typeRecords = (intersectRecords `on` typeRecords) a b,
  typeConstructors = (intersectConstructors `on` typeConstructors) a b
}

intersectRecords :: TypeRecord -> TypeRecord -> TypeRecord
intersectRecords (TypeRecord as) (TypeRecord bs) =
  TypeRecord $ join (liftM2 intersectPositiveRecords as bs)

intersectPositiveRecords :: TypeRecordGroup -> TypeRecordGroup -> [TypeRecordGroup]
intersectPositiveRecords a b = pure $ Map.intersectionWith intersectType a b

intersectConstructors :: TypeConstructors -> TypeConstructors -> TypeConstructors
intersectConstructors (TypeConstructors a) (TypeConstructors b) =
  TypeConstructors $ liftA2 intersectConstructorFunctions a b
intersectConstructors (TypeConstructors a) (TypeConstructorsComplement b) =
  undefined
intersectConstructors a@(TypeConstructorsComplement _) b@(TypeConstructors _) =
  intersectConstructors b a
intersectConstructors (TypeConstructorsComplement a) (TypeConstructorsComplement b) =
  undefined

intersectConstructorFunctions :: (Type -> Type) -> (Type -> Type) -> (Type -> Type)
intersectConstructorFunctions a b x = intersectType (a x) (b x)

complementType :: Type -> Type
complementType = undefined

isNever :: Type -> Bool
isNever (Type {
  typeNominal = TypeNominal ns,
  typeRecords = TypeRecord rs,
  typeConstructors = TypeConstructors con
}) = null ns && null rs && isNothing con
isNever _ = False
