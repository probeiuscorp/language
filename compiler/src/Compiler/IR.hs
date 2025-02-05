{-# LANGUAGE OverloadedStrings #-}

module Compiler.IR where

import qualified Compiler.AST as AST
import Compiler.Modules (TillyModuleBuildable)
import LLVM.AST.Operand (Operand(ConstantOperand, LocalReference))
import LLVM.AST.Constant (Constant(Undef, GlobalReference, Int))
import LLVM.IRBuilder.Module (ModuleBuilder, function, extern, buildModule, ParameterName (NoParameterName))
import LLVM.IRBuilder.Monad (IRBuilderT, named, freshUnName)
import LLVM.IRBuilder.Constant (int32, int64, double)
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.AST.Type as Type
import LLVM.AST (Module)
import Control.Monad (foldM, forM_, void)
import Control.Monad.State.Strict (MonadTrans (lift))
import Data.Foldable (Foldable(toList))
import GHC.Num (integerFromInt)
import Data.String (IsString(fromString))
import qualified Data.Map as Map

type Codegen = IRBuilderT ModuleBuilder

userIdentifier ident = fromString $ 'u' : '_' : ident
-- llvm-hs tries to unique names, so the first unique identifier will have an "_0" appended.
-- However, it does not reverse this process, so it has to be done here instead.
userReference :: AST.ValidIdentifier -> Operand
userReference = LocalReference anyValueType . userIdentifier . (++ "_0")
emitExpr :: AST.Expression -> Codegen Operand
emitExpr (AST.ExprIdentifier ident) = pure $ userReference ident
emitExpr (AST.ExprApplication termTarget termArgument) = do
  target <- emitExpr termTarget
  argument <- emitExpr termArgument
  functionCall target argument
emitExpr (AST.ExprFunction free destruct body) = functionExpression destruct free body
emitExpr (AST.ExprIntegral int) = valueOf KindInt $ int64 $ integerFromInt int
emitExpr (AST.ExprDouble dbl) = valueOf KindDouble $ double dbl
emitExpr _ = undefined

mallocType = Type.FunctionType Type.ptr [Type.i64] False
malloc :: Type.Type -> Codegen Operand
malloc ty = do
  size <- L.sizeof 64 ty
  L.call mallocType (ConstantOperand $ GlobalReference "malloc") [(size, [])]

undef = ConstantOperand . Undef
structure = Type.StructureType False
zipWithIndices :: Enum b => b -> [a] -> [(a, b)]
zipWithIndices n = flip zip [n..]
buildStruct :: Type.Type -> [Operand] -> Codegen Operand
buildStruct recordType = foldM (\currentRecord (operand, i) ->
    L.insertValue currentRecord operand [i]
  ) (undef recordType) . zipWithIndices 0

anyValueType :: Type.Type
anyValueType = structure [Type.i64, Type.i64]
data ValueKind = KindClosure | KindData | KindRecord | KindInt | KindDouble deriving (Eq, Ord, Show, Enum)
kindOf :: ValueKind -> Operand
kindOf = int64 . integerFromInt . fromEnum
valueOf :: ValueKind -> Operand -> Codegen Operand
valueOf kind operand = buildStruct anyValueType [kindOf kind, operand]

functionExpression :: AST.Destructuring -> AST.VarSet -> AST.Expression -> Codegen Operand
functionExpression (AST.DestructBind bind) free body = do
  nextName <- freshUnName
  fn <- lift $ def nextName
  closure <- malloc envType
  L.store closure 0 fn
  forM_ (zipWithIndices 1 $ toList free) $ \(ident, i) -> do
    addr <- getAddr closure i
    L.store addr 0 $ userReference ident
  closureInt <- L.ptrtoint closure Type.i64
  valueOf KindClosure closureInt
  where
    getAddr operand i = L.gep Type.ptr operand [ConstantOperand $ Int 32 i]
    envType = structure $ Type.ptr : (anyValueType <$ toList free)
    def name = function name [(Type.ptr, NoParameterName), (anyValueType, userIdentifier bind)] anyValueType $ \case
      [env, _] -> do
        forM_ (zipWithIndices 0 $ toList free) $ \(ident, i) -> do
          addr <- getAddr env i
          L.load anyValueType addr 0 `named` userIdentifier ident
        res <- emitExpr body
        L.ret res
      _ -> undefined
functionExpression _ _ _ = undefined

fnType = Type.FunctionType anyValueType [Type.ptr, anyValueType] False
functionCall :: Operand -> Operand -> Codegen Operand
functionCall value argument = do
  closureData <- L.extractValue value [1]
  closurePtr <- L.inttoptr closureData Type.ptr
  fnPtrPtr <- L.gep Type.ptr closurePtr [int32 0]
  fnPtr <- L.load Type.ptr fnPtrPtr 0
  envPtr <- L.gep Type.ptr closurePtr [int32 1]
  L.call fnType fnPtr [(envPtr, []), (argument, [])]

mkMainModule :: TillyModuleBuildable -> Module
mkMainModule (_, exprs) = buildModule "main" $ do
  void $ extern "malloc" [Type.i64] Type.ptr
  forM_ (Map.toList exprs) $ \(ident, expr) -> do
    function (userIdentifier ident) [] anyValueType $ const $ do
      res <- emitExpr expr
      L.ret res
