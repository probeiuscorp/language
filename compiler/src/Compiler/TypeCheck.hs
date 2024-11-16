module Compiler.TypeCheck where

import qualified Compiler.AST as AST
import Compiler.Type
import Control.Applicative (Applicative(liftA2))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Function (on)
import Control.Monad (join, liftM2)
import Data.Maybe (isNothing)
import Compiler.Zipper (filterMaybe)

inferType :: AST.Expression -> Type
inferType = undefined

intersectType :: Type -> Type -> Type
intersectType a b = Type {
  typeNominal = (intersectNominals `on` typeNominal) a b,
  typeRecords = (intersectRecords `on` typeRecords) a b,
  typeConstructors = (intersectConstructors `on` typeConstructors) a b
}

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 f g a b = f $ g a b
intersectNominals :: TypeNominal -> TypeNominal -> TypeNominal
intersectNominals (TypeNominal a) (TypeNominal b) = TypeNominal $ Set.intersection a b
intersectNominals (TypeNominal a) (TypeNominalComplement b) = differenceNominals a b
intersectNominals (TypeNominalComplement a) (TypeNominal b) = differenceNominals b a
intersectNominals (TypeNominalComplement a) (TypeNominalComplement b) = TypeNominalComplement $ Set.union a b
differenceNominals :: Set.Set NominalType -> Set.Set NominalType -> TypeNominal
differenceNominals = compose2 TypeNominal Set.difference

intersectRecords :: TypeRecord -> TypeRecord -> TypeRecord
intersectRecords (TypeRecord as) (TypeRecord bs) =
  TypeRecord $ join (liftM2 intersectPositiveRecords as bs)
intersectRecords (TypeRecordComplement as) (TypeRecordComplement bs) =
  TypeRecordComplement $ as ++ bs
intersectRecords (TypeRecord as) (TypeRecordComplement bs) = intersectPositiveWithNegative as bs
intersectRecords (TypeRecordComplement as) (TypeRecord bs) = intersectPositiveWithNegative bs as

intersectPositiveRecords :: TypeRecordGroup -> TypeRecordGroup -> [TypeRecordGroup]
intersectPositiveRecords a b = pure $ Map.intersectionWith intersectType a b
intersectPositiveWithNegative :: TypeRecordGroups -> TypeRecordGroups -> TypeRecord
intersectPositiveWithNegative poss negs = TypeRecord $ (\pos -> foldr intersectOneNegativeWithOnePositive pos negs) <$> poss
intersectOneNegativeWithOnePositive :: TypeRecordGroup -> TypeRecordGroup -> TypeRecordGroup
intersectOneNegativeWithOnePositive neg pos = Map.differenceWith aExceptB pos neg
aExceptB :: Type -> Type -> Maybe Type
aExceptB a b = filterMaybe (not . isNever) $ intersectType a $ complementType b

intersectConstructors :: TypeConstructors -> TypeConstructors -> TypeConstructors
intersectConstructors (TypeConstructors a) (TypeConstructors b) =
  TypeConstructors $ liftA2 intersectConstructorFunctions a b
intersectConstructors (TypeConstructors a) (TypeConstructorsComplement b) = differenceConstructors a b
intersectConstructors (TypeConstructorsComplement a) (TypeConstructors b) = differenceConstructors b a
intersectConstructors (TypeConstructorsComplement a) (TypeConstructorsComplement b) =
  TypeConstructorsComplement $ liftA2 intersectConstructorComplementFunctions a b

intersectConstructorFunctions :: (Type -> Type) -> (Type -> Type) -> (Type -> Type)
intersectConstructorFunctions a b x = intersectType (a x) (b x)
intersectConstructorComplementFunctions :: (Type -> Type) -> (Type -> Type) -> (Type -> Type)
intersectConstructorComplementFunctions a b x = unionType (a x) (b x)
differenceConstructors :: Maybe (Type -> Type) -> Maybe (Type -> Type) -> TypeConstructors
differenceConstructors m@(Just _) Nothing = TypeConstructors m
differenceConstructors _ _ = TypeConstructors Nothing

complementType :: Type -> Type
complementType (Type {
  typeNominal = typeNominal,
  typeRecords = typeRecords,
  typeConstructors = typeConstructors
}) = Type {
  typeNominal = complementNominal typeNominal,
  typeRecords = complementRecords typeRecords,
  typeConstructors = complementConstructors typeConstructors
}
complementNominal :: TypeNominal -> TypeNominal
complementNominal (TypeNominal a) = TypeNominalComplement a
complementNominal (TypeNominalComplement a) = TypeNominal a
complementRecords :: TypeRecord -> TypeRecord
complementRecords (TypeRecord a) = TypeRecordComplement a
complementRecords (TypeRecordComplement a) = TypeRecord a
complementConstructors :: TypeConstructors -> TypeConstructors
complementConstructors (TypeConstructors a) = TypeConstructorsComplement a
complementConstructors (TypeConstructorsComplement a) = TypeConstructors a

isNever :: Type -> Bool
isNever (Type {
  typeNominal = TypeNominal ns,
  typeRecords = TypeRecord rs,
  typeConstructors = TypeConstructors con
}) = null ns && null rs && isNothing con
isNever _ = False

unionType :: Type -> Type -> Type
unionType = compose2 complementType (intersectType `on` complementType)
