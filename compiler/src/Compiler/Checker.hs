module Compiler.Checker where

import qualified Compiler.AST as AST
import Compiler.Type

inferType :: AST.Expression -> Type
inferType = undefined

intersectType :: Type -> Type -> Type
intersectType = undefined

complementType :: Type -> Type
complementType = undefined

isNever :: Type -> Bool
isNever = undefined
