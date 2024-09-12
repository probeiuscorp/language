{-# LANGUAGE RecordWildCards #-}

module Compiler.Type where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Function (on)

data NominalType = NominalType
  deriving (Eq, Ord, Show)

-- | Types as written in source file.
data TypeTerm

-- | Type where bindings have been resolved.
data TypeExpression
  = TyExUnknown
  | TyExNever
  | TyExNominal NominalType
  | TyExRecord (Map.Map String TypeExpression)
  | TyExTypeConstructor (TypeExpression -> TypeExpression)
  | TyExApplication TypeExpression TypeExpression
  | TyExIntersection TypeExpression TypeExpression
  | TyExComplement TypeExpression
  | TyExTilde TypeExpression

data TypeNominal
  = TypeNominal (Set.Set NominalType)
  | TypeNominalComplement (Set.Set NominalType)
  deriving (Eq, Show)

type TypeRecordFields = [Map.Map String Type]
data TypeRecord
  = TypeRecord TypeRecordFields
  | TypeRecordComplement TypeRecordFields
  deriving (Eq, Show)

data TypeConstructors
  = TypeConstructors (Maybe (Type -> Type))
  | TypeConstructorsComplement (Maybe (Type -> Type))

-- | Normalized type.
data Type = Type
  { typeNominal :: TypeNominal
  , typeRecords :: TypeRecord
  , typeConstructors :: TypeConstructors
  }

normalizeType :: TypeExpression -> Type
normalizeType = undefined


data ShowableTypeConstructors
  = ShowableTypeConstructors (Maybe String)
  | ShowableTypeConstructorsComplement (Maybe String)
  deriving (Eq, Show)
data ShowableType = ShowableType
  { shTypeNominal :: TypeNominal
  , shTypeRecords :: TypeRecord
  , shTypeConstructors :: ShowableTypeConstructors
  }
  deriving (Eq, Show)

showableType :: Type -> ShowableType
showableType Type{..} = ShowableType
  { shTypeNominal = typeNominal
  , shTypeRecords = typeRecords
  , shTypeConstructors = case typeConstructors of
      TypeConstructors con -> ShowableTypeConstructors $ showCon con
      TypeConstructorsComplement con -> ShowableTypeConstructorsComplement $ showCon con
  }
  where showCon = ("<some fn>" <$)

instance Eq Type where
  (==) = (==) `on` showableType
instance Show Type where
  show = show . showableType
