{-# LANGUAGE RankNTypes #-}

module Compiler.Modules where

import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Tokenize (tokenize)
import Compiler.Semantic (semanticValue)
import Compiler.Parse (splitDeclarations, parseDeclaration)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State (evalState)
import Control.Lens
import Data.Bool (bool)
import Data.Validation(fromEither, toEither)

newtype ModuleIdentifier = ModuleIdentifier { unModuleIdentifier :: String } deriving (Eq, Ord, Show)

type TillyModuleParsed = ([(String, AST.ImportListing)], Set.Set AST.ValidIdentifier, Map.Map AST.ValidIdentifier AST.Term)
parModImports = _1
parModExposed = _2
parModBindings = _3

type TillyModuleBuildable = ((), Map.Map AST.ValidIdentifier AST.Expression)

parseModule :: String -> TillyModuleParsed
parseModule source = foldr (maybe id $ flip foldDeclaration) m0 declarations
  where
    m0 :: TillyModuleParsed
    m0 = (,,) mempty mempty mempty
    declarations = evalState parseDeclaration . Z.start <$> (splitDeclarations . Z.start . tokenize $ source)
    foldDeclaration :: TillyModuleParsed -> AST.TopLevelDeclaration -> TillyModuleParsed
    foldDeclaration m = ($ m) . \case
      AST.ImportDeclaration specifier listing -> over parModImports ((specifier, listing) :)
      AST.ValueDeclaration (AST.DeclarationModule ident isExported) maybeValue _ ->
        bool id (over parModExposed (Set.insert ident)) isExported .
        maybe id (over parModBindings . Map.insert ident) maybeValue
      _ -> error "unsupported declaration"

verifyModuleBuildable :: TillyModuleParsed -> Either [AST.ParseError] TillyModuleBuildable
verifyModuleBuildable (_, _, terms) = ((),) <$> exprs
  where
    exprs = toEither $ traverse (fromEither . semanticValue knownVars) terms
    knownVars = Map.keysSet terms
