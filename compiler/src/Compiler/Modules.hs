{-# LANGUAGE RankNTypes #-}

module Compiler.Modules where

import Compiler.Prelude
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.ModuleTypes
import Compiler.Tokenize (tokenize)
import Compiler.Semantic (semanticValue, collectBindings)
import Compiler.Parse (splitDeclarations, parseDeclaration, TopLevelDeclaration, ParseContext)
import Compiler.Interpret (intrinsicsModule)
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
import Data.Foldable (foldrM)
import Data.Maybe (isJust)
import Data.List (isPrefixOf)

type Validated = Writer [AST.ParseError]
type ValidatedT = WriterT [AST.ParseError]
data TillyModuleAccum = TillyModuleAccum
  { _accModIdentifier :: ModuleIdentifier
  , _accModImports :: [(ModuleSpecifier, AST.ImportListing)]
  , _accModExposed :: Set.Set AST.ValidIdentifier
  , _accModBindings :: Map.Map AST.ValidIdentifier (ParseContext -> AST.Term)
  , _accModFixities :: [(AST.ValidIdentifier, AST.Fixity)]
  }
makeLenses ''TillyModuleAccum

identifierFromSpecifier :: ModuleIdentifier -> ModuleSpecifier -> ModuleIdentifier
identifierFromSpecifier baseModule specifier
  | "." `isPrefixOf` unModuleSpecifier specifier = LocalFileModule . normalise $ takeDirectory (unModuleIdentifier baseModule) </> unModuleSpecifier specifier
  | otherwise = NamedModule $ unModuleSpecifier specifier

findModules :: ModuleIdentifier -> Map.Map ModuleIdentifier TillyModuleParsed -> ValidatedT IO (Map.Map ModuleIdentifier TillyModuleParsed)
findModules identifier foundModules = case tryReadModule identifier of
  Nothing -> pure foundModules  -- The intrinsics module is already found
  Just ioContents -> do
    parsed <- WriterT $ runWriter . parseModule identifier <$> ioContents
    let imports = view parModImports parsed
    let withModule = Map.insert identifier parsed foundModules
    (\f -> foldrM f withModule imports) $ \(specifier, _) acc -> let modId = identifierFromSpecifier identifier specifier in
      if Map.member modId acc
        then pure acc
        else findModules modId acc

parseModule :: ModuleIdentifier -> String -> Validated TillyModuleParsed
parseModule modid source = processModule $ foldr (maybe id $ flip foldDeclaration) m0 declarations
  where
    m0 :: TillyModuleAccum
    m0 = TillyModuleAccum modid mempty mempty mempty mempty
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
processModule m = TillyModuleParsed (m^.accModIdentifier) (view accModImports m) (view accModExposed m) <$> addFixities (view accModFixities m)
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

type ModuleScope = Map.Map AST.ValidIdentifier (ModuleIdentifier, Maybe AST.Fixity)
getModuleScope :: TillyModuleParsed -> Map.Map ModuleIdentifier TillyModule -> ModuleScope
getModuleScope parsedModule modules = ownScope <> foldMap scopeFromListing imports
  where
    getmap m = fst <$> m^.parModBindings
    ownIdentifier = parsedModule^.parModIdentifier
    ownScope = (ownIdentifier,) <$> getmap parsedModule
    imports = view parModImports parsedModule
    scopeFromListing (specifier, listing) = let
      modid = identifierFromSpecifier ownIdentifier specifier
      m = modules Map.! modid
      exposed = getExposed m
      in Map.fromSet (\ident -> (modid, getFixityForBinding m ident)) $ case listing of
        AST.ImportAll -> exposed
        AST.ImportAs as -> Set.singleton as
        AST.ImportOnly destruct -> collectBindings destruct
        AST.ImportHiding destruct -> exposed Set.\\ collectBindings destruct

reverseMap :: (Ord k, Ord a) => Map.Map k a -> Map.Map a (Set.Set k)
reverseMap inputMap = (\f -> Map.foldrWithKey f mempty inputMap) $ \value key -> flip Map.alter key $
  Just . maybe (Set.singleton value) (Set.insert value)

verifyModuleBuildable :: ModuleScope -> TillyModuleParsed -> Validation [AST.ParseError] TillyModuleBuildable
verifyModuleBuildable moduleScope m = TillyModuleBuildable neededScope <$> exprs
  where
    neededScope = Map.delete (m^.parModIdentifier) $ reverseMap $ fst <$> moduleScope
    terms = view parModBindings m
    knownVars = Map.keysSet moduleScope
    exprs = traverse (fromEither . semanticValue aboutOperators knownVars . ($ aboutOperators) . snd) terms
    aboutOperators = fromMaybe AST.defaultFixity . (snd <=< flip Map.lookup moduleScope)

theIntrinsicsModule = IntrinsicModule (Map.keysSet intrinsicsModule)
buildModules :: ModuleIdentifier -> IO (Either [AST.ParseError] TillyBuildOutputs)
buildModules mainModuleId = do
  (userModules, errs) <- runWriterT $ findModules mainModuleId mempty
  let modules = Map.insert intrinsicsModuleIdentifier theIntrinsicsModule $ ParsedModule <$> userModules
  let buildables = (`traverse` userModules) $ \userModule -> verifyModuleBuildable (getModuleScope userModule modules) userModule
  let built = (mainModuleId,) <$> buildables
  pure . toEither $ built <* (if null errs then Success () else Failure errs)
