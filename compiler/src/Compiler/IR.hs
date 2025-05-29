{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler.IR where

import qualified Compiler.AST as AST
import Compiler.Modules (TillyModuleBuildable)
import LLVM.AST.Operand (Operand(ConstantOperand, LocalReference))
import LLVM.AST.Constant (Constant(Undef, GlobalReference, Int))
import LLVM.IRBuilder.Module (ModuleBuilder, function, extern, buildModule, ParameterName (NoParameterName))
import LLVM.IRBuilder.Monad (IRBuilderT, named, freshUnName, block)
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
import qualified Data.Set as Set

type Codegen = IRBuilderT ModuleBuilder

userIdentifier ident = fromString $ 'u' : '_' : ident
-- llvm-hs tries to unique names, so the first unique identifier will have an "_0" appended.
-- However, it does not reverse this process, so it has to be done here instead.
userReference :: AST.ValidIdentifier -> Operand
userReference = LocalReference anyValueType . userIdentifier . (++ "_0")
emitExpr :: AST.Expression -> Codegen Operand
emitExpr expr = defer (collectFreeVariablesExpr expr) $ getExpr expr
  where
    getExpr = \case
      (AST.ExprIdentifier ident) -> pure $ userReference ident
      (AST.ExprApplication termTarget termArgument) -> do
        target <- emitExpr termTarget
        argument <- emitExpr termArgument
        functionCall target argument
      (AST.ExprFunction free destruct body) -> functionExpression destruct free body
      (AST.ExprIntegral int) -> valueOf KindInt $ int64 $ integerFromInt int
      (AST.ExprDouble dbl) -> valueOf KindDouble $ double dbl
      _ -> undefined

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
data ValueKind = KindThunk | KindClosure | KindData | KindRecord | KindInt | KindDouble deriving (Eq, Ord, Show, Enum)
kindOf :: ValueKind -> Operand
kindOf = int64 . integerFromInt . fromEnum
valueOf :: ValueKind -> Operand -> Codegen Operand
valueOf kind operand = buildStruct anyValueType [kindOf kind, operand]
ptrtoint :: Operand -> Codegen Operand
ptrtoint operand = L.ptrtoint operand Type.i64
inttoptr :: Operand -> Codegen Operand
inttoptr operand = L.inttoptr operand Type.ptr

prepEnv :: [AST.ValidIdentifier] -> (Integer -> Integer) -> (Operand -> Codegen (), Operand -> Codegen ())
prepEnv idents offset = (writeEnv, readEnv)
  where
    xs = zipWithIndices 0 idents
    getAddr operand i = L.gep Type.ptr operand [int32 $ offset i]
    writeEnv target = forM_ xs $ \(ident, i) -> do
      addr <- getAddr target i
      L.store addr 0 $ userReference ident
    readEnv env = forM_ xs $ \(ident, i) -> do
      addr <- getAddr env i
      L.load anyValueType addr 0 `named` userIdentifier ident

functionExpression :: AST.Destructuring -> AST.VarSet -> AST.Expression -> Codegen Operand
functionExpression (AST.DestructBind bind) freeset body = do
  nextName <- freshUnName
  fn <- lift $ def nextName
  closure <- malloc envType
  L.store closure 0 fn
  writeEnv closure
  valueOf KindClosure =<< ptrtoint closure
  where
    free = toList freeset
    envType = structure $ Type.ptr : (anyValueType <$ free)
    (writeEnv, readEnv) = prepEnv free (+1)
    def name = function name [(Type.ptr, NoParameterName), (anyValueType, userIdentifier bind)] anyValueType $ \case
      [env, _] -> do
        readEnv env
        L.ret =<< emitExpr body
      _ -> undefined
functionExpression _ _ _ = undefined

fnType = Type.FunctionType anyValueType [Type.ptr, anyValueType] False
functionCall :: Operand -> Operand -> Codegen Operand
functionCall value argument = do
  closure <- inttoptr =<< L.extractValue value [1]
  fnPtrPtr <- L.gep Type.ptr closure [int32 0]
  fnPtr <- L.load Type.ptr fnPtrPtr 0
  envPtr <- L.gep Type.ptr closure [int32 1]
  L.call fnType fnPtr [(envPtr, []), (argument, [])]

getThunkStatusPtr thunk = L.gep Type.ptr thunk [int32 0]
getThunkEnvOrKindPtr thunk = L.gep Type.ptr thunk [int32 1]
getThunkFnOrContentPtr thunk = L.gep Type.ptr thunk [int32 2]
thunkSuspended = Int 1 0
thunkEvaluated = Int 1 1
defer :: AST.VarSet -> Codegen Operand -> Codegen Operand
defer freeset body = do
  let free = toList freeset
  let envType = structure $ anyValueType <$ free
  let (writeEnv, readEnv) = prepEnv free id
  let thunkType = structure [Type.i1, Type.ptr, Type.ptr]
  name <- freshUnName
  fn <- lift $ function name [(Type.ptr, NoParameterName)] anyValueType $ \case
    [env] -> do
      readEnv env
      L.ret =<< body
    _ -> undefined
  thunk <- malloc thunkType
  thunkStatusPtr <- getThunkStatusPtr thunk
  thunkEnvOrKindPtr <- getThunkEnvOrKindPtr thunk
  thunkFnOrContentPtr <- getThunkFnOrContentPtr thunk
  env <- malloc envType
  writeEnv env
  L.store thunkStatusPtr 0 $ ConstantOperand thunkSuspended
  L.store thunkEnvOrKindPtr 0 env
  L.store thunkFnOrContentPtr 0 fn
  valueOf KindThunk =<< ptrtoint thunk

demand :: Operand -> Codegen Operand
demand argument = mdo
  thunk <- inttoptr =<< L.extractValue argument [1]
  thunkStatusPtr <- getThunkStatusPtr thunk
  thunkEnvOrKindPtr <- getThunkEnvOrKindPtr thunk
  thunkFnOrContentPtr <- getThunkFnOrContentPtr thunk
  thunkStatus <- L.load Type.i1 thunkStatusPtr 0
  envOrKind <- L.load Type.i64 thunkEnvOrKindPtr 0
  fnOrContent <- L.load Type.ptr thunkFnOrContentPtr 0
  L.switch thunkStatus ifSuspended [(thunkEvaluated, ifEvaluated)]
  --
  ifSuspended <- block
  let env = envOrKind; fn = fnOrContent
  suspendedValue <- L.call (Type.FunctionType anyValueType [Type.ptr] False) fn [(env, [])]
  gotKind <- L.extractValue suspendedValue [0]
  gotContent <- L.extractValue suspendedValue [1]
  L.store thunkStatusPtr 0 $ ConstantOperand thunkEvaluated
  L.store thunkEnvOrKindPtr 0 gotKind
  L.store thunkFnOrContentPtr 0 gotContent
  L.br merge
  --
  ifEvaluated <- block
  let kind = envOrKind
  content <- ptrtoint fnOrContent
  evaluatedValue <- buildStruct anyValueType [kind, content]
  L.br merge
  --
  merge <- block
  L.phi [(suspendedValue, ifSuspended), (evaluatedValue, ifEvaluated)]

-- | For development. At some point this needs to be moved into the AST itself.
collectFreeVariablesExpr :: AST.Expression -> AST.VarSet
collectFreeVariablesExpr = \case
  AST.ExprFunction varset _ _ -> varset
  AST.ExprApplication f x -> collectFreeVariablesExpr f <> collectFreeVariablesExpr x
  AST.ExprIdentifier ident -> Set.singleton ident
  AST.ExprList els -> collect els
  AST.ExprTuple els -> collect els
  _ -> Set.empty
  where collect = foldMap collectFreeVariablesExpr

mkMainModule :: TillyModuleBuildable -> Module
mkMainModule (_, exprs) = buildModule "main" $ do
  void $ extern "malloc" [Type.i64] Type.ptr
  forM_ (Map.toList exprs) $ \(ident, expr) -> do
    function (userIdentifier ident) [] anyValueType $ const $ do
      L.ret =<< emitExpr expr
  function "demand" [(anyValueType, NoParameterName)] anyValueType $ \[thunk] -> do
    L.ret =<< demand thunk
