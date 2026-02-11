{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler.IR where

import Compiler.Prelude
import qualified Compiler.AST as AST
import Compiler.Modules (TillyModuleBuildable)
import LLVM.AST.Operand (Operand(ConstantOperand, LocalReference))
import LLVM.AST.Constant (Constant(Undef, GlobalReference, Int, Struct, PtrToInt))
import LLVM.IRBuilder.Module (function, extern, ParameterName (NoParameterName), ModuleBuilderT, buildModuleT, global)
import LLVM.IRBuilder.Monad (IRBuilderT, named, block)
import LLVM.AST.Name (Name)
import LLVM.IRBuilder.Constant (int32, int64, double)
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.AST.Type as Type
import LLVM.AST (Module, Name (Name))
import Control.Monad (foldM, forM_)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.State.Strict (MonadTrans (lift), State, evalState, MonadState (state))
import GHC.Num (integerFromInt)
import Data.String (IsString(fromString))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (inits)
import Compiler.Semantic (collectBindings)

-- | The state is the next global name. The supply of freshUnName is not preserved
-- between `lift`s. The State is innermost to be preserved while dropping IRBuilderT.
type ModuleCodegen = ModuleBuilderT (ReaderT (AST.VarSet, AST.ValidIdentifier -> Integer) (State Int))
type Codegen = IRBuilderT ModuleCodegen
askKnownGlobals = asks fst
askTagOfConstructor = asks snd

userIdentifier ident = fromString $ 'u' : '_' : ident
-- llvm-hs tries to unique names, so the first unique identifier will have an "_0" appended.
-- However, it does not reverse this process, so it has to be done here instead.
userReference :: AST.ValidIdentifier -> Codegen Operand
userReference ident = do
  globalSet <- askKnownGlobals
  if ident `Set.member` globalSet
    then L.load anyValueType (ConstantOperand . GlobalReference . userIdentifier $ ident) 0
    else pure . LocalReference anyValueType . userIdentifier . (++ "_0") $ ident
emitExpr :: AST.Expression -> Codegen Operand
emitExpr expr = defer freeset $ getExpr expr
  where
    freeset = collectFreeVariablesExpr expr
    getExpr = \case
      (AST.ExprIdentifier ident) -> userReference ident
      (AST.ExprApplication termTarget termArgument) -> do
        target <- emitExpr termTarget
        argument <- emitExpr termArgument
        functionCall target argument
      (AST.ExprFunction free destruct body) -> functionExpression destruct free $ emitExpr body
      (AST.ExprIntegral int) -> valueOf KindInt $ int64 $ integerFromInt int
      (AST.ExprDouble dbl) -> valueOf KindDouble $ double dbl
      (AST.ExprMatch clauses) -> matchExpression clauses freeset
      _ -> undefined

mallocType = Type.FunctionType Type.ptr [Type.i64] False
malloc :: Type.Type -> Codegen Operand
malloc ty = do
  size <- L.sizeof 64 ty
  L.call mallocType (ConstantOperand $ GlobalReference "malloc") [(size, [])]

undef = ConstantOperand . Undef
structure = Type.StructureType False
taggedType ty xs = structure $ ty : (anyValueType <$ xs)
ptrTaggedType = taggedType Type.ptr
dataType = taggedType Type.i64
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

prepEnv :: Type.Type -> [AST.ValidIdentifier] -> (Integer -> Integer) -> (Operand -> Codegen (), Operand -> Codegen ())
prepEnv envType idents offset = (writeEnv, readEnv)
  where
    xs = zipWithIndices 0 idents
    getAddr operand i = L.gep envType operand [int32 0, int32 $ offset i]
    writeEnv target = forM_ xs $ \(ident, i) -> do
      addr <- getAddr target i
      L.store addr 0 =<< userReference ident
    readEnv env = forM_ xs $ \(ident, i) -> do
      addr <- getAddr env i
      L.load anyValueType addr 0 `named` userIdentifier ident

nextName :: MonadState Int m => m Name
nextName = state $ \x -> (Name $ fromString $ 'g' : '_' : show x, x + 1)

emitFunction :: Foldable t => t AST.ValidIdentifier -> (Operand -> Codegen Operand) -> Codegen Operand
emitFunction freeset receive = do
  fnName <- nextName
  fn <- lift $ def fnName
  closure <- malloc envType
  L.store closure 0 fn
  writeEnv closure
  valueOf KindClosure =<< ptrtoint closure
  where
    free = toList freeset
    envType = ptrTaggedType free
    (writeEnv, readEnv) = prepEnv envType free (+1)
    def name = function name [(Type.ptr, NoParameterName), (anyValueType, NoParameterName)] anyValueType $ \case
      [env, parameter] -> do
        readEnv env
        L.ret =<< receive parameter
      _ -> undefined

functionExpression :: Foldable t => AST.Destructuring -> t AST.ValidIdentifier -> Codegen Operand -> Codegen Operand
functionExpression destruct freeset body = emitFunction freeset $ \param -> loadDestruct destruct param *> body

-- | Straightforward but silly way to introduce an named alias to `parameter`
introduceAlias :: AST.ValidIdentifier -> Operand -> Codegen ()
introduceAlias ident op = void $ mdo
  L.br x; x <- block
  L.br y; y <- block
  L.phi [(op, x)] `named` userIdentifier ident
loadDestruct :: AST.Destructuring -> Operand -> Codegen ()
loadDestruct (AST.DestructBind ident) parameter = introduceAlias ident parameter
loadDestruct (AST.DestructAs ident destruct) parameter = do
  introduceAlias ident parameter
  loadDestruct destruct parameter
loadDestruct (AST.DestructNominal _ positions) parameter = do
  dataBodyPtr <- inttoptr =<< flip L.extractValue [1] =<< demand parameter
  forM_ (zipWithIndices 1 positions) $ \(destruct, i) -> do
    ptr <- L.gep (dataType positions) dataBodyPtr [int32 0, int32 i]
    loadDestruct destruct =<< L.load anyValueType ptr 0
loadDestruct _ _ = undefined

fnType = Type.FunctionType anyValueType [Type.ptr, anyValueType] False
functionCall :: Operand -> Operand -> Codegen Operand
functionCall thunk argument = do
  value <- demand thunk
  closure <- inttoptr =<< L.extractValue value [1]
  fnPtrPtr <- L.gep (structure [Type.ptr]) closure [int32 0, int32 0]
  fnPtr <- L.load Type.ptr fnPtrPtr 0
  L.call fnType fnPtr [(closure, []), (argument, [])]

matchExpression :: [(AST.Destructuring, AST.Expression)] -> AST.VarSet -> Codegen Operand
matchExpression clauses free = emitFunction free $ \param -> mdo
  let entryEnum = fromString . show <$> zipWith const [(0 :: Integer)..] clauses
  let endLabel = "unreachable"
  let entryNames = zip entryEnum $ drop 1 entryEnum ++ [endLabel]
  nodes <- foldM (appendClause param end) [] (zip entryNames clauses)
  _ <- block `named` endLabel
  L.unreachable
  end <- block
  L.phi nodes
  where
    appendClause param endLabel xs ((n1, n2), (destruct, expr)) = mdo
      L.br tryClause
      tryClause <- block `named` n1
      case destruct of
        AST.DestructBind _ -> pure ()
        -- TODO: recursive matching
        AST.DestructAs _ _ -> pure ()
        AST.DestructNominal constructor positions -> mdo
          getWantTag <- askTagOfConstructor
          dataBlockPtr <- inttoptr =<< flip L.extractValue [1] =<< demand param
          tagPtr <- L.gep (dataType positions) dataBlockPtr [int32 0, int32 0]
          tag <- L.load Type.i64 tagPtr 0
          L.switch tag (Name $ n2 <> "_0") [(Int 64 $ getWantTag constructor, ifMatch)]
          ifMatch <- block
          pure ()
        _ -> undefined
      loadDestruct destruct param
      result <- emitExpr expr
      L.br after
      after <- block
      L.br endLabel
      pure ((result, after) : xs)

thunkType = structure [Type.i1, Type.ptr, Type.ptr]
getThunkStatusPtr thunk = L.gep thunkType thunk [int32 0, int32 0]
getThunkEnvOrKindPtr thunk = L.gep thunkType thunk [int32 0, int32 1]
getThunkFnOrContentPtr thunk = L.gep thunkType thunk [int32 0, int32 2]
thunkSuspended = Int 1 0
thunkEvaluated = Int 1 1
defer :: Foldable t => t AST.ValidIdentifier -> Codegen Operand -> Codegen Operand
defer freeset body = do
  let free = toList freeset
  let envType = structure $ anyValueType <$ free
  let (writeEnv, readEnv) = prepEnv envType free id
  name <- nextName
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
deferGlobal :: AST.ValidIdentifier -> Codegen Operand -> ModuleCodegen ()
deferGlobal ident body = do
  fnName <- nextName
  -- TODO: this is quite some duplication with `defer`. Unify.
  -- thunk with no captures
  void $ function fnName [(anyValueType, NoParameterName)] anyValueType $ const $ do
    L.ret =<< body
  thunkName <- nextName
  void $ global thunkName thunkType $ Struct Nothing False
    [thunkSuspended, GlobalReference fnName, GlobalReference fnName]
  void $ global (userIdentifier ident) anyValueType $ Struct Nothing False
    [Int 64 . integerFromInt . fromEnum $ KindThunk, PtrToInt (GlobalReference thunkName) Type.i64]

demandReference :: Name
demandReference = "demand"
demand :: Operand -> Codegen Operand
demand operand = L.call (Type.FunctionType anyValueType [anyValueType] False) (ConstantOperand $ GlobalReference demandReference) [(operand, [])]

demandRoutine :: Operand -> Codegen Operand
demandRoutine argument = mdo
  tag <- L.extractValue argument [0]
  L.switch tag notThunk [(Int 64 . integerFromInt $ fromEnum KindThunk, ifThunk)]
  --
  ifThunk <- block
  thunk <- inttoptr =<< L.extractValue argument [1]
  thunkStatusPtr <- getThunkStatusPtr thunk
  thunkEnvOrKindPtr <- getThunkEnvOrKindPtr thunk
  thunkFnOrContentPtr <- getThunkFnOrContentPtr thunk
  thunkStatus <- L.load Type.i1 thunkStatusPtr 0
  envOrKind <- L.load Type.ptr thunkEnvOrKindPtr 0
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
  L.br mergeThunk
  --
  ifEvaluated <- block
  kind <- ptrtoint envOrKind
  content <- ptrtoint fnOrContent
  evaluatedValue <- buildStruct anyValueType [kind, content]
  L.br mergeThunk
  --
  mergeThunk <- block
  thunkValue <- demand =<< L.phi [(suspendedValue, ifSuspended), (evaluatedValue, ifEvaluated)]
  L.br merge
  --
  notThunk <- block
  L.br merge
  --
  merge <- block
  L.phi [(argument, notThunk), (thunkValue, mergeThunk)]

-- | For development. At some point this needs to be moved into the AST itself.
collectFreeVariablesExpr :: AST.Expression -> AST.VarSet
collectFreeVariablesExpr = \case
  AST.ExprFunction varset _ _ -> varset
  AST.ExprApplication f x -> collectFreeVariablesExpr f <> collectFreeVariablesExpr x
  AST.ExprIdentifier ident -> Set.singleton ident
  AST.ExprList els -> collect els
  AST.ExprTuple els -> collect els
  AST.ExprMatch clauses -> collect (snd <$> clauses) Set.\\ foldMap (collectBindings . fst) clauses
  _ -> Set.empty
  where collect = foldMap collectFreeVariablesExpr

mkMainModule :: TillyModuleBuildable -> Module
mkMainModule (_, exprs) = flip evalState 0 . flip runReaderT readerContext . buildModuleT "main" $ do
  void $ extern "malloc" [Type.i64] Type.ptr
  forM_ dataDeclarations $ \((fnIdent, arity), tag) -> do
    let names = ('_' :) . show <$> take arity [0..] :: [AST.ValidIdentifier]
    let sCaptures = inits names
    let is = pure
    deferredData <- is $ defer names $ do
      let dataType = structure $ Type.i64 : (anyValueType <$ names)
      dataBlock <- malloc dataType
      L.store dataBlock 0 $ int64 tag
      forM_ (zip names [1..]) $ \(pIdent, i) -> do
        ptr <- L.gep dataType dataBlock [int32 0, int32 i]
        L.store ptr 0 =<< userReference pIdent
      valueOf KindData =<< ptrtoint dataBlock
    deferGlobal fnIdent $ (\f -> foldr f deferredData (zip names sCaptures)) $ \(name, captures) ->
      functionExpression (AST.DestructBind name) captures
  forM_ (Map.toList exprs) $ \(ident, expr) -> do
    deferGlobal ident $ emitExpr expr
  function demandReference [(anyValueType, NoParameterName)] anyValueType $ \[thunk] -> do
    L.ret =<< demandRoutine thunk
  function "evaluateClosure" [(anyValueType, NoParameterName), (anyValueType, NoParameterName)] anyValueType $ \[f, x] -> do
    L.ret =<< functionCall f x
  function "valueOfChar" [(Type.i64, NoParameterName)] anyValueType $ \[ch] -> do
    L.ret =<< valueOf KindInt ch
  where
    readerContext = (globalIdents, (Map.fromList (first fst <$> dataDeclarations) Map.!))
    globalIdents = Map.keysSet exprs <> Set.fromList (fst <$> dataDeclarationsNoTags)
    dataIO = [("IOmap", 2), ("IOapply", 2), ("IOjoin", 1), ("getLine", 0), ("putStrLn", 1)]
    dataStandard = [("Unit", 0), ("Some", 1), ("None", 0), ("Cons", 2), ("Nil", 0)]
    dataDeclarationsNoTags = dataIO <> dataStandard :: [(AST.ValidIdentifier, Int)]
    dataDeclarations = zip dataDeclarationsNoTags [0..]
