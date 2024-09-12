module Compiler.Checker where

import qualified Compiler.AST as AST
import Compiler.Type
import Control.Applicative (Applicative(liftA2))
import Data.Function (on)

inferType :: AST.Expression -> Type
inferType = undefined

intersectType :: Type -> Type -> Type
intersectType a b = Type {
  typeNominal = undefined,
  typeRecords = undefined,
  typeConstructors = (intersectConstructors `on` typeConstructors) a b
}
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
isNever = undefined
