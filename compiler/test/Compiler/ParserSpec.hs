module Compiler.ParserSpec (spec) where

import Test.Hspec
import Compiler.Parser
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Tokenizer (tokenize)

spec :: SpecWith ()
spec = describe "Compiler.Parser" $ do
  describe "parseImportDeclaration" $ do
    let testImportDeclaration = parseImportDeclaration . Z.start . tokenize
    it "should parse import all" $ do
      fmap (fmap fst) (testImportDeclaration "import react/hooks") `shouldBe` Just
        (Right $ AST.ImportDeclaration "react/hooks" AST.ImportAll)
    it "should parse import as" $ do
      fmap (fmap fst) (testImportDeclaration "import react/hooks as Hooks") `shouldBe` Just
        (Right $ AST.ImportDeclaration "react/hooks" $ AST.ImportAs "Hooks")
