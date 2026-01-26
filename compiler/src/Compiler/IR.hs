{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler.IR where

import qualified Compiler.AST as AST
import Compiler.Modules (TillyModuleBuildable)
import LLVM.AST.Operand (Operand(ConstantOperand, LocalReference))
import LLVM.AST.Constant (Constant(Undef, GlobalReference, Int, Struct, PtrToInt))
import LLVM.IRBuilder.Module (function, extern, ParameterName (NoParameterName), ModuleBuilderT, buildModuleT, global)
import LLVM.IRBuilder.Monad (IRBuilderT, named, block, freshUnName, emitInstr)
import LLVM.AST.Name (Name)
import qualified LLVM.AST.IntegerPredicate as Predicate
import LLVM.IRBuilder.Constant (int32, int64, double)
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.AST.Type as Type
import LLVM.AST (Module, Name (Name))
import qualified Control.Lens as Lens
import Control.Monad (foldM, forM_, void)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.State.Strict (MonadTrans (lift), State, evalState, MonadState (state))
import Data.Foldable (Foldable(toList))
import GHC.Num (integerFromInt)
import Data.String (IsString(fromString))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (inits, sortOn)
import Data.Bifunctor (Bifunctor(first))
import Compiler.Semantic (collectBindings)

-- | The state is the next global name. The supply of freshUnName is not preserved
-- between `lift`s. The State is innermost to be preserved while dropping IRBuilderT.
type ModuleCodegen = ModuleBuilderT (ReaderT CodegenContext (State Int))
type Codegen = IRBuilderT ModuleCodegen
type CodegenContext = (AST.VarSet, AST.ValidIdentifier -> Integer, Map.Map String Integer)
askKnownGlobals = asks $ Lens.view Lens._1
askTagOfConstructor = asks $ Lens.view Lens._2
askMemberIdByString = asks $ (Map.!) . Lens.view Lens._3

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
      (AST.ExprRecord members) -> emitRecord members
      (AST.ExprMemberAccess target member) -> emitMemberAccess target member
      (AST.ExprMatch clauses) -> matchExpression clauses freeset
      _ -> undefined

mallocType = Type.FunctionType Type.ptr [Type.i64] False
mallocRaw size = L.call mallocType (ConstantOperand $ GlobalReference "malloc") [(size, [])]
malloc :: Type.Type -> Codegen Operand
malloc ty = mallocRaw =<< L.sizeof 64 ty

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

anyValueTypePositions :: [Type.Type]
anyValueTypePositions = [Type.i64, Type.i64]
anyValueType :: Type.Type
anyValueType = structure anyValueTypePositions
data ValueKind = KindThunk | KindClosure | KindData | KindRecord | KindInt | KindDouble
  deriving (Eq, Ord, Show, Enum)
kindOf :: ValueKind -> Operand
kindOf = int64 . integerFromInt . fromEnum
valueOf :: ValueKind -> Operand -> Codegen Operand
valueOf = valueFrom . kindOf
valueFrom :: Operand -> Operand -> Codegen Operand
valueFrom kind operand = buildStruct anyValueType [kind, operand]
ptrtoint, inttoptr, getDataAsPtr :: Operand -> Codegen Operand
ptrtoint operand = L.ptrtoint operand Type.i64
inttoptr operand = L.inttoptr operand Type.ptr
getDataAsPtr op = inttoptr =<< L.extractValue ??? [1] =<< demand op

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

emitRecord :: [(String, AST.Expression)] -> Codegen Operand
emitRecord members = do
  keyId <- askMemberIdByString
  -- Each member gets the 1) key id; 2) kind; 3) data
  let recordType = structure $ Type.i64 : (members *> (Type.i64 : anyValueTypePositions))
  record <- malloc recordType
  L.store record 0 $ int64 $ fromIntegral $ length members
  forM_ (sortOn (keyId.fst.fst) $ zipWithIndices 0 members) $ \((key, expr), i) -> do
    let iStart = i * 3 + 1
    value <- emitExpr expr `named` userIdentifier key
    -- TODO: can this be memcpy?
    a1 <- L.extractValue value [0]
    a2 <- L.extractValue value [1]
    let positions = [int64 $ keyId key, a1, a2]
    forM_ (zipWithIndices 0 positions) $ \(op, offset) -> do
      ptr <- L.gep recordType record [int32 0, int32 (iStart + offset)]
      L.store ptr 0 op
  valueOf KindRecord =<< ptrtoint record

(???) = flip
infix 9 ???
data DeconsRecord = DeconsRecord
  { dcrRecord :: Operand
  , dcrSize :: Codegen Operand
  , dcrWriteSize :: Operand -> Codegen ()
  , dcrReadSlot :: Operand -> Codegen (Operand, Operand, Operand)
  , dcrWriteSlot :: Operand -> (Operand, Operand, Operand) -> Codegen ()
  }
deconsRecord :: Operand -> DeconsRecord
deconsRecord record = DeconsRecord record size writeSize readSlot writeSlot
  where
    unknownRecordType = Type.VectorType 32 Type.i64
    size = do
      ptr <- L.gep unknownRecordType record [int32 0, int32 1]
      L.load Type.i64 ptr 0
    writeSize recordSize = do
      L.store record 0 recordSize
    getPtrs i = do
      iId <- L.add (int64 2) =<< L.mul i (int64 3)
      iKind <- L.add iId $ int64 1
      iData <- L.add iId $ int64 2
      seqt3 $ (iId, iKind, iData) `pipet3` \iPos ->
        L.gep unknownRecordType record [int32 0, iPos]
    readSlot i = do
      ptrs <- getPtrs i
      seqt3 $ ptrs `pipet3` L.load Type.i64 ??? 0
    writeSlot i ops = do
      ptrs <- getPtrs i
      void . seqt3 $ zipt3 ops ptrs `pipet3` \(v, ptr) ->
        L.store ptr 0 v
    infix 6 `pipet3`
    pipet3 (a, b, c) f = (f a, f b, f c)
    seqt3 :: Applicative f => (f a, f b, f c) -> f (a, b, c)
    seqt3 (ma, mb, mc) = (,,) <$> ma <*> mb <*> mc
    zipt3 (a, b, c) (d, e, f) = ((a, d), (b, e), (c, f))
emitMemberAccess :: AST.Expression -> String -> Codegen Operand
emitMemberAccess expr member = mdo
  keyId <- askMemberIdByString
  let recordType = Type.ArrayType 0 Type.i64
  record <- getDataAsPtr =<< emitExpr expr
  L.br before
  before <- block
  L.br loop
  loop <- block
  i <- L.phi [(int64 1, before), (iNext, loop)]
  iNext <- L.add i $ int64 3
  tagCheckPtr <- L.gep recordType record [int32 0, i]
  tagCheck <- L.load Type.i64 tagCheckPtr 0
  isZero <- L.icmp Predicate.EQ tagCheck $ int64 $ keyId member
  L.condBr isZero done loop
  done <- block
  i1 <- L.add i $ int64 1
  i2 <- L.add i $ int64 2
  p1 <- L.gep recordType record [int32 0, i1]
  p2 <- L.gep recordType record [int32 0, i2]
  v1 <- L.load Type.i64 p1 0
  v2 <- L.load Type.i64 p2 0
  valueFrom v1 v2

-- | Meld intrinsic. Combines two records into one, with preference for members of RHS.
-- Record members are stored in increasing order of member ids. Simple linear merge.
meld :: Operand -> Operand -> Codegen Operand
meld left' right' = mdo
  left <- deconsRecord <$> getDataAsPtr left'
  leftSize <- dcrSize left
  right <- deconsRecord <$> getDataAsPtr right'
  rightSize <- dcrSize right
  -- TODO: Currently over-allocating. Better can be done.
  -- basicSize <- L.sizeof 64 Type.i64
  outPtr <- mallocRaw =<< L.mul (int64 8) =<< L.add (int64 1) =<< L.mul (int64 3) =<< L.add leftSize rightSize
  let out = deconsRecord outPtr
  let write = dcrWriteSlot out k
  L.br before
  before <- block
  L.br loop

  loop <- block
  -- i tracks row index in left, j in right, k in out
  i <- L.phi [(int64 0, before), (LocalReference Type.i64 "ims_0", "latch_0")]
  j <- L.phi [(int64 0, before), (LocalReference Type.i64 "jms_0", "latch_0")]
  k <- L.phi [(int64 0, before), (LocalReference Type.i64 "kms_0", "latch_0")]
  isDone <- bindM2 L.and (L.icmp Predicate.EQ i leftSize) (L.icmp Predicate.EQ j rightSize)
  i0 <- L.add i $ int64 0
  j0 <- L.add j $ int64 0
  k0 <- L.add k $ int64 0
  iP1 <- L.add i0 $ int64 1
  jP1 <- L.add j0 $ int64 1
  L.condBr isDone done notDone
  notDone <- block
  readLHS@(idLHS, _, _) <- dcrReadSlot left i
  readRHS@(idRHS, _, _) <- dcrReadSlot right j
  shouldOverwrite <- L.icmp Predicate.EQ idLHS idRHS
  L.condBr shouldOverwrite isEQ isNEQ
  isNEQ <- block
  shouldUniqueLHS <- L.icmp Predicate.ULT idLHS idRHS
  L.condBr shouldUniqueLHS isUniqueLHS isUniqueRHS

  isEQ <- block
  write readRHS
  L.br latch
  isUniqueLHS <- block
  write readLHS
  L.br latch
  isUniqueRHS <- block
  write readRHS
  L.br latch
  latch <- block `named` "latch"
  _ <- L.phi [(iP1, isEQ), (iP1, isUniqueLHS), (i0, isUniqueRHS)] `named` "ims"
  _ <- L.phi [(jP1, isEQ), (j0, isUniqueLHS), (jP1, isUniqueRHS)] `named` "jms"
  _ <- L.add k0 (int64 1) `named` "kms"
  L.br loop

  done <- block
  dcrWriteSize out $ LocalReference Type.i64 "kms_0"
  valueOf KindRecord =<< ptrtoint (dcrRecord out)
  where
    bindM2 fm ma mb = do { a <- ma; b <- mb; fm a b }

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

generateIntrinsics :: ModuleCodegen ()
generateIntrinsics = mdo
  meldRef <- function "meld" [(anyValueType, NoParameterName), (anyValueType, NoParameterName)] anyValueType $ \[lhs, rhs] -> do
    L.ret =<< meld lhs rhs
  deferGlobal "meld" $ emitFunction [] $ \lhs -> emitFunction [] $ \rhs ->
    L.call (Type.FunctionType anyValueType [anyValueType, anyValueType] False) meldRef [(lhs, mempty), (rhs, mempty)]

mkMainModule :: TillyModuleBuildable -> Module
mkMainModule (_, outs) = flip evalState 0 . flip runReaderT readerContext . buildModuleT "main" $ do
  void $ extern "malloc" [Type.i64] Type.ptr
  generateIntrinsics
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
    exprs = fst <$> outs
    memberIdByKey = Map.fromList $ zipWithIndices (0 :: Integer) $ concatMap (toList . snd) outs
    readerContext = (globalIdents, (Map.fromList (first fst <$> dataDeclarations) Map.!), memberIdByKey)
    globalIdents = Set.insert "meld" $ Map.keysSet exprs <> Set.fromList (fst <$> dataDeclarationsNoTags)
    dataIO = [("IOmap", 2), ("IOapply", 2), ("IOjoin", 1), ("getLine", 0), ("putStrLn", 1)]
    dataStandard = [("Unit", 0), ("Some", 1), ("None", 0), ("Cons", 2), ("Nil", 0)]
    dataDeclarationsNoTags = dataIO <> dataStandard :: [(AST.ValidIdentifier, Int)]
    dataDeclarations = zip dataDeclarationsNoTags [0..]
