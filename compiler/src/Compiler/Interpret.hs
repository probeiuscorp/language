{-# LANGUAGE RankNTypes #-}

module Compiler.Interpret (interpretMainFile) where

import Compiler.Prelude
import Control.Lens
import Compiler.Modules
import qualified Compiler.AST as AST
import qualified Data.Map as Map
import Data.List (intercalate)

data Value
  = VIntrinsic String
  | VRecord (Map.Map String Value)
  | VData Integer [Value]
  | VFunction (Value -> Value)
  | VChar Char
  | VInteger Integer
  | VDouble Double
$(makePrisms ''Value)
showValue :: Value -> String
showValue (VIntrinsic ident) = "intrinsic(" <> ident <> ")"
showValue (VRecord fields) = "record{" <> intercalate "," (Map.keys fields) <> "}"
showValue (VData tag _) = "data[" <> show tag <> "]"
showValue (VFunction _) = "function"
showValue (VChar ch) = "char(" <> pure ch <> ")"
showValue (VInteger integer) = "integer(" <> show integer <> ")"
showValue (VDouble double) = "double(" <> show double <> ")"
typeError expected value = error $ "type error: expected " <> expected <> " but got " <> showValue value
expectType :: Prism' Value a -> Value -> a
expectType ty value = case preview ty value of
  Nothing -> typeError "double" value
  Just received -> received

intrinsicsModule = Map.fromList
  [ ("add_int", hom _VInteger (+))
  , ("add_double", hom _VDouble (+))
  , ("meld", hom _VRecord (flip Map.union))
  , ("io_pure", VFunction $ \v -> VData ioPure [v])
  , ("io_map", VFunction $ \f -> VFunction $ \ma -> VData ioMap [f, ma])
  , ("io_join", VFunction $ \mma -> VData ioJoin [mma])
  , ("io_putStrLn", VFunction $ \str -> VData ioPutStrLn [str])
  , ("io_getLine", VData ioGetLine [])
  ]
  where
    hom :: Prism' Value a -> (a -> a -> a) -> Value
    hom ty f = VFunction $ \v1 -> VFunction $ \v2 -> review ty $ expectType ty v1 `f` expectType ty v2

type ModuleEvaluationContext = Map.Map AST.ValidIdentifier Integer
type ValueScope = Map.Map AST.ValidIdentifier Value
-- | Evaluates to NF
evaluate :: ModuleEvaluationContext -> ValueScope -> AST.Expression -> Value
evaluate ctx scope0 = \case
  (AST.ExprIdentifier ident) -> mustFind ident scope0
  (AST.ExprApplication f body) -> expectType _VFunction (go f) (go body)
  (AST.ExprDouble double) -> VDouble double
  (AST.ExprIntegral integer) -> VInteger $ fromIntegral integer
  (AST.ExprList list) -> toTillyList $ go <$> list
  (AST.ExprTuple slots) -> tuple $ go <$> slots
  (AST.ExprRecord record) -> VRecord $ Map.fromList $ second go <$> record
  (AST.ExprFunction _ destruct0 expr) -> VFunction $ \v -> r (collectBindings v scope0 destruct0) expr
  (AST.ExprMatch clauses) -> VFunction $ \subject -> let
      -- TODO: should recursively match
      tryMatch :: (AST.Destructuring, AST.Expression) -> Maybe Value
      tryMatch (destruct@(AST.DestructNominal dataKind _), body) = let (subjectTag, _) = expectType _VData subject in
        if subjectTag == ctx Map.! dataKind
          then Just (r (collectBindings subject scope0 destruct) body)
          else Nothing
      tryMatch (destruct, body) = Just (r (collectBindings subject scope0 destruct) body)
      triedMatches = tryMatch <$> clauses
    in case asum triedMatches of
      Just matchingValue -> matchingValue
      Nothing -> error $ "pattern match against " <> showValue subject <> " failed"
  where
    r = evaluate ctx
    go = r scope0
    mustFind ident scope = case Map.lookup ident scope of
      Just value -> value
      Nothing -> error $ "undefined reference " <> ident
    collectBindings :: Value -> ValueScope -> AST.Destructuring -> ValueScope
    collectBindings val scope = \case
      AST.DestructBind ident -> Map.insert ident val scope
      AST.DestructAs ident destruct -> Map.insert ident val (collectBindings val scope destruct)
      AST.DestructNominal _ destructs -> let (_, slots) = expectType _VData val in
        -- Maps are left biased but we want right bias
        (`foldMap` reverse (zip destructs slots)) $ \(destruct, value) -> collectBindings value scope destruct
      AST.DestructRecord rows -> let matchingAgainst = expectType _VRecord val in
        (`foldMap` rows) $ \(ident, mDestruct) -> let rowValue = mustFind ident matchingAgainst in
          maybe (Map.singleton ident rowValue) (collectBindings rowValue scope) mDestruct

-- Data tags
tupleOfSize n = -(n + 1)
tuple values = VData (tupleOfSize $ toInteger $ length values) values
ioPure = 0
ioMap = 1
ioJoin = 2
ioPutStrLn = 3
ioGetLine = 4
tagCons = 5
tagNil = 6
-- | Executes the Tilly IO, assuming it is an IO
execute :: Value -> IO Value
execute (VData 0 [value]) = pure value
execute (VData 1 [f, ma]) = expectType _VFunction f <$> execute ma
execute (VData 2 [mma]) = execute =<< execute mma
execute (VData 3 [str]) = tuple [] <$ putStrLn (expectType _VChar <$> fromTillyList str)
execute (VData 4 []) = toTillyList . fmap VChar <$> getLine
execute value = typeError "IO" value

fromTillyList :: Value -> [Value]
fromTillyList (VData 5 [char, xs]) = char : fromTillyList xs
fromTillyList (VData 6 []) = []
fromTillyList value = typeError "List" value

toTillyList :: [Value] -> Value
toTillyList (x : xs) = VData tagCons [x, toTillyList xs]
toTillyList [] = VData tagNil []

interpretMainFile :: TillyModuleBuildable -> IO ()
interpretMainFile tlModule = do
  putStrLn "began evaluation..."
  let exposedExprs = tlModule^.tlExposedExprs
  let
    evaluateWithScope = evaluate mempty mainModuleScope
    mainModuleScope = intrinsicsModule <> (evaluateWithScope <$> exposedExprs)
  case Map.lookup "main" mainModuleScope of
    Nothing -> putStrLn "Add a `main` to your main module"
    Just mainIO -> void $ execute mainIO
