module Compiler.ModuleTypes where

import Compiler.Prelude
import Compiler.Parse (ParseContext)
import qualified Compiler.AST as AST
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath ((</>))

newtype ModuleSpecifier = ModuleSpecifier { unModuleSpecifier :: String }
  deriving (Eq, Ord, Show)
data ModuleIdentifier
  = LocalFileModule FilePath
  | NamedModule String
  deriving (Eq, Ord, Show)
intrinsicsModuleIdentifier = NamedModule "std/intrinsics"
unModuleIdentifier = \case
  LocalFileModule filepath -> filepath
  NamedModule name -> name
tryReadModule :: ModuleIdentifier -> Maybe (IO String)
tryReadModule = \case
  (LocalFileModule filepath) -> Just $ readFile filepath
  ((== intrinsicsModuleIdentifier) -> True) -> Nothing
  (NamedModule name) -> Just $ readFile $ "/home/caleb/language/compiler/packages/" </> name

data TillyModuleParsed = TillyModuleParsed
  { _parModIdentifier :: ModuleIdentifier
  , _parModImports :: [(ModuleSpecifier, AST.ImportListing)]
  , _parModExposed :: Set.Set AST.ValidIdentifier
  , _parModBindings :: Map.Map AST.ValidIdentifier (Maybe AST.Fixity, ParseContext -> AST.Term)
  }
makeLenses ''TillyModuleParsed

data TillyModule = ParsedModule TillyModuleParsed | IntrinsicModule (Set.Set AST.ValidIdentifier)
getExposed :: TillyModule -> AST.VarSet
getExposed (ParsedModule m) = m^.parModExposed
getExposed (IntrinsicModule varSet) = varSet
getFixityForBinding :: TillyModule -> AST.ValidIdentifier -> Maybe AST.Fixity
getFixityForBinding (ParsedModule m) ident = fst =<< Map.lookup ident $: m^.parModBindings
getFixityForBinding _ _ = Nothing

data TillyModuleBuildable = TillyModuleBuildable
  { _tlExposedExprs :: Map.Map AST.ValidIdentifier AST.Expression
  }
makeLenses ''TillyModuleBuildable
