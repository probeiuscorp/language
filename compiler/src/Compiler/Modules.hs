{-# LANGUAGE RankNTypes #-}

module Compiler.Modules where

import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Tokenize (tokenize)
import Compiler.Semantic (semanticValue, collectBindings)
import Compiler.Parse (splitDeclarations, parseDeclaration, TopLevelDeclaration, ParseContext)
import qualified Compiler.Parse as AST
import System.FilePath (takeDirectory, normalise, (</>))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State (evalState)
import Control.Monad.Writer (Writer, tell, runWriter)
import Control.Monad.Trans.Writer (WriterT (WriterT, runWriterT))
import Control.Lens
import Data.Bool (bool)
import Data.Validation(fromEither, Validation (Success, Failure), toEither)
import Data.Foldable (foldrM, Foldable (toList))
import Data.Maybe (isJust)
import Control.Monad (join)

newtype ModuleSpecifier = ModuleSpecifier { unModuleSpecifier :: String }
  deriving (Eq, Ord, Show)
newtype ModuleIdentifier = ModuleIdentifier { unModuleIdentifier :: FilePath }
  deriving (Eq, Ord, Show)
readModule = readFile . unModuleIdentifier

type Validated = Writer [AST.ParseError]
type ValidatedT = WriterT [AST.ParseError]
data TillyModuleAccum = TillyModuleAccum
  { _accModImports :: [(ModuleSpecifier, AST.ImportListing)]
  , _accModExposed :: Set.Set AST.ValidIdentifier
  , _accModBindings :: Map.Map AST.ValidIdentifier (ParseContext -> AST.Term)
  , _accModFixities :: [(AST.ValidIdentifier, AST.Fixity)]
  }
makeLenses ''TillyModuleAccum
data TillyModuleParsed = TillyModuleParsed
  { _parModImports :: [(ModuleSpecifier, AST.ImportListing)]
  , _parModExposed :: Set.Set AST.ValidIdentifier
  , _parModBindings :: Map.Map AST.ValidIdentifier (Maybe AST.Fixity, ParseContext -> AST.Term)
  }
makeLenses ''TillyModuleParsed
type TillyModuleBuildable = ((), Map.Map AST.ValidIdentifier AST.Expression)

identifierFromSpecifier :: ModuleIdentifier -> ModuleSpecifier -> ModuleIdentifier
identifierFromSpecifier baseModule specifier = ModuleIdentifier . normalise $ takeDirectory (unModuleIdentifier baseModule) </> unModuleSpecifier specifier

findModules :: ModuleIdentifier -> Map.Map ModuleIdentifier TillyModuleParsed -> ValidatedT IO (Map.Map ModuleIdentifier TillyModuleParsed)
findModules identifier foundModules = do
  parsed <- WriterT $ runWriter . parseModule <$> readModule identifier
  let imports = view parModImports parsed
  let withModule = Map.insert identifier parsed foundModules
  (\f -> foldrM f withModule imports) $ \(specifier, _) acc -> let modId = identifierFromSpecifier identifier specifier in
    if Map.member modId acc
      then pure acc
      else findModules modId acc

parseModule :: String -> Validated TillyModuleParsed
parseModule source = processModule $ foldr (maybe id $ flip foldDeclaration) m0 declarations
  where
    m0 :: TillyModuleAccum
    m0 = TillyModuleAccum mempty mempty mempty mempty
    declarations = evalState parseDeclaration . Z.start <$> (splitDeclarations . Z.start . tokenize $ source)
    foldDeclaration :: TillyModuleAccum -> TopLevelDeclaration -> TillyModuleAccum
    foldDeclaration m = ($ m) . \case
      Left (AST.ImportDeclaration specifier listing) -> over accModImports ((ModuleSpecifier specifier, listing) :)
      Left (AST.InfixDeclaration specifier fixity) -> over accModFixities ((specifier, fixity) :)
      Right (AST.ValueDeclaration (AST.DeclarationModule ident isExported) value) ->
        bool id (over accModExposed (Set.insert ident)) isExported .
        over accModBindings (Map.insert ident value)
      Right (AST.DataDeclaration _ _) -> id
      _ -> error "unsupported declaration"

processModule :: TillyModuleAccum -> Validated TillyModuleParsed
processModule m = TillyModuleParsed (view accModImports m) (view accModExposed m) <$> addFixities (view accModFixities m)
  where
    bindings = view accModBindings m
    addFixities :: [(AST.ValidIdentifier, AST.Fixity)] -> Validated (Map.Map AST.ValidIdentifier (Maybe AST.Fixity, ParseContext -> AST.Term))
    addFixities = foldrM thing ((Nothing,) <$> bindings)
      where
        x y k = if k then Just y else Nothing
        thing (ident, fixity) acc = Map.adjust (\(_, term) -> (Just fixity, term)) ident acc <$ tell (toList =<<
          [ x (AST.ErrFixityDeclarationSubjectNotLocal ident) (Map.notMember ident bindings)
          , x (AST.ErrFixityDeclarationOverwriting ident) (isJust $ fst =<< ident `Map.lookup` acc)
          ])

type ModuleScope = Map.Map AST.ValidIdentifier (Maybe AST.Fixity)
getModuleScope :: Map.Map ModuleIdentifier TillyModuleParsed -> ModuleIdentifier -> ModuleScope
getModuleScope modules identifier = ownScope <> foldMap scopeFromListing imports
  where
    getmap m = fst <$> view parModBindings m
    ownScope = getmap parsedModule
    parsedModule = modules Map.! identifier
    imports = view parModImports parsedModule
    scopeFromListing (specifier, listing) = let
      modId = identifierFromSpecifier identifier specifier
      m = modules Map.! modId
      exposed = view parModExposed m
      in Map.fromSet (getmap m Map.!) $ case listing of
        AST.ImportAll -> exposed
        AST.ImportAs as -> Set.singleton as
        AST.ImportOnly destruct -> collectBindings destruct
        AST.ImportHiding destruct -> exposed Set.\\ collectBindings destruct

verifyModuleBuildable :: ModuleScope -> TillyModuleParsed -> Validation [AST.ParseError] TillyModuleBuildable
verifyModuleBuildable moduleScope m = ((),) <$> exprs
  where
    terms = view parModBindings m
    knownVars = Map.keysSet moduleScope <> Set.fromList ["IOmap", "IOjoin", "getLine", "putStrLn"]
    exprs = traverse (fromEither . semanticValue knownVars . ($ aboutOperators) . snd) terms
    aboutOperators = join . flip Map.lookup moduleScope

buildModules :: ModuleIdentifier -> IO (Either [AST.ParseError] TillyModuleBuildable)
buildModules modid = do
  (modules, errs) <- runWriterT $ findModules modid mempty
  let vBuildable = verifyModuleBuildable (getModuleScope modules modid) $ modules Map.! modid
  pure . toEither $ vBuildable <* (if null errs then Success () else Failure errs)
