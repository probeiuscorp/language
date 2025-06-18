module Compiler.ParseSpec (spec, prettyPrintTerm, testAboutOperators) where

import Test.Hspec
import Compiler.Parse
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Tokenize (tokenize)
import Compiler.Linearize (linearize)
import Compiler.SnapshotTesting (snapshot, prettyShow)
import Control.Monad.State (evalState)

parens str = "(" ++ str ++ ")"
prettyPrintTerm :: String -> AST.Term -> String
prettyPrintTerm lastIndent (AST.TermApplication (AST.TermApplication fn arg1) arg2) =
  prettyPrintTerm lastIndent fn
  ++ "\n" ++ indent ++ prettyPrintTerm (lastIndent ++ "\x2502 ") arg1
  ++ "\n" ++ indent ++ prettyPrintTerm (lastIndent ++ "  ") arg2
  where indent = lastIndent ++ "\x2937 "
prettyPrintTerm indent (AST.TermApplication fn arg) = "(" ++ prettyPrintTerm indent fn ++ " " ++ prettyPrintTerm indent arg ++ ")"
prettyPrintTerm _ (AST.TermIdentifier ident) = ident
prettyPrintTerm lastIndent (AST.TermFunction params body) = "TermFunction " ++ show params ++ " (\n" ++ indent ++ prettyPrintTerm indent body ++ "\n" ++ lastIndent ++ ")"
  where indent = "  " ++ lastIndent
prettyPrintTerm lastIndent (AST.TermWhere body clauses) = "TermWhere (\n" ++ indent ++ prettyPrintTerm indent body ++ "\n" ++ lastIndent ++ ") [\n" ++ (clauses >>= showEntry) ++ lastIndent ++ "]"
  where
    showEntry (destruct, clause) = indent ++ show destruct ++ " = " ++ prettyPrintTerm indent clause ++ "\n"
    indent = "  " ++ lastIndent
prettyPrintTerm lastIndent (AST.TermMultilineOperator op lines) = "TermMultilineOperator " ++ parens (show op) ++ "\n" ++ (lines >>= ((lastIndent ++ "\x2014 ") ++) . (++ "\n") . prettyPrintTerm ("  " ++ lastIndent))
prettyPrintTerm indent (AST.TermRecord fields) = "TermRecord {\n" ++ (fields >>= showField) ++ indent ++ "}"
  where
    nextIndent = "  " ++ indent
    showValue Nothing = "Nothing"
    showValue (Just term) = prettyPrintTerm nextIndent term
    showField (name, value) = nextIndent ++ name ++ " = " ++ showValue value ++ "\n"
prettyPrintTerm indent (AST.TermMatch clauses) = "TermMatch [\n" ++ (clauses >>= showField) ++ indent ++ "]"
  where
    nextIndent = "  " ++ indent
    showField (destructurings, term) = nextIndent ++ show destructurings ++ " = " ++ prettyPrintTerm nextIndent term ++ "\n"
prettyPrintTerm _ term@(AST.TermStringLiteral _) = parens $ show term
prettyPrintTerm _ term = show term

prettyPrintDeclaration :: TopLevelDeclaration -> String
prettyPrintDeclaration (Left x) = prettyShow x
prettyPrintDeclaration (Right (DataDeclaration decl mt)) = prettyShow ("DataDeclaration", decl, ($ testAboutOperators) <$> mt)
prettyPrintDeclaration (Right (ValueDeclaration decl t)) = prettyShow ("ValueDeclaration", decl, t testAboutOperators)
prettyPrintDeclaration (Right (TypeDeclaration decl t)) = prettyShow ("TypeDeclaration", decl, t testAboutOperators)

opFixity :: String -> AST.Fixity
opFixity "¬" = AST.FixityPrefix
opFixity "~" = AST.FixityPrefix
opFixity "!" = AST.FixityPostfix
opFixity "?" = AST.FixityPostfix
opFixity ident = AST.FixityInfix $ AST.Infix (getOpPrecedence ident) (getOpAssociativity ident)

getOpPrecedence :: String -> Double
getOpPrecedence "$" = 1
getOpPrecedence "-" = 4
getOpPrecedence "^" = 8
getOpPrecedence "^^" = 8
getOpPrecedence _ = 6

getOpAssociativity :: String -> AST.Associativity
getOpAssociativity "$" = AST.RightAssociative
getOpAssociativity "-" = AST.LeftAssociative
getOpAssociativity "*" = AST.LeftAssociative
getOpAssociativity "/" = AST.LeftAssociative
getOpAssociativity "\\" = AST.NonAssociative
getOpAssociativity _ = AST.RightAssociative

testAboutOperators = Just . opFixity

spec :: SpecWith ()
spec = describe "Compiler.Parse" $ do
  describe "splitDeclarations" $ do
    let test = snapshot "parse/splitDeclarations/" $ prettyShow . splitDeclarations . Z.start . tokenize
    test "empty string" ""
    test "just newlines" "\n\n\n\n\n\n"
    test "continue on inline whitespace" "pair = a.\n\t(a, a)"
    test "continue on closing brace" "import react/hooks {\n\tuseState\n}\n"
    test "continue on closing parenthesis" "pair = a. (\n\ta,\n\ta)\n"
    test "basic multi declaration" "import react {\n\tComponentsProps\n}\n\nimport react/hooks {\n\tuseState\n}\n"
  let prettyParseDeclaration = (\case Just x -> prettyPrintDeclaration x; Nothing -> "Nothing") . evalState parseDeclaration . Z.start . tokenize
  describe "parseImportDeclaration" $ do
    let test = snapshot "parse/parseImport/" prettyParseDeclaration
    test "import all" "import react/hooks"
    test "import only" "import react/hooks {}"
    test "import only list" "import react/hooks { useState, useEffect }"
    test "import only list trailing whitespace" "import react/hooks { useState, useEffect }\n"
    test "import hiding list" "import react/hooks hiding { useState, useEffect }"
    test "import as" "import react/hooks as ReactHooks"
  describe "binding declarations" $ do
    let test = snapshot "parse/declaration binding/" prettyParseDeclaration
    test "simple" "true = x y. x"
    test "exported" "export main = pure ()"
  describe "data declarations" $ do
    let test = snapshot "parse/declaration data/" prettyParseDeclaration
    test "no body" "data None"
    test "no body exported" "export data None"
    test "body" "data Maybe = a. Some a + None"
    test "body exported" "export data Maybe = a. Some a + None"
  let source = Z.start . linearize . Z.start . tokenize
  describe "infix declarations" $ do
    let test = snapshot "parse/declaration infix/" prettyParseDeclaration
    test "left associative" "infixl 1 $$"
    test "right associative" "infixr 1 $$"
    test "non associative" "infix 1 \\"
    test "fractional precedence" "infixr 2.110 |"
    test "complex precedence" "infixr 0x20 |"
    test "prefix declaration" "prefix ¬"
    test "postfix declaration" "postfix !"
  describe "parseDestructuring" $ do
    let prettyParseDestructuring = show . evalState parseDestructuring . source
    let test = snapshot "parse/parseDestructuring/" prettyParseDestructuring
    test "binding" "x"
    test "nominal" "(None)"
    test "nominal many" "(Cons x xs)"
    test "nominal nested" "(Cons (Nothing) (Just x))"
    test "record" "{ x, y, z }"
    test "record rebind" "{ x as playerX, y as playerY }"
    test "record destructure" "{ start as { x as x0, y as y0 }, end as { x as x1, y as y1 } }"
    test "as pattern" "list@(Cons x xs)"
    test "as pattern record" "{ pos as pos@{ x, y } }"
  describe "parseTerm" $ do
    let prettyParseTerm = prettyPrintTerm "" . parseTerm testAboutOperators . source
    let test = snapshot "parse/parseTerm/" prettyParseTerm
    test "identity function" "x. x"
    test "multiple bindings function" "x y. x"
    test "single group" "(this)"
    test "tree group" "(this that there)"
    test "tuple unit" "()"
    test "tuple unit whitespace" "(  )"
    test "tuple 2" "(this, that)"
    test "tuple 3" "(this, that, there)"
    test "tuple section" "(this, , there)"
    test "list one item" "[x]"
    test "list multiple items" "[x, y, z]"
    test "nested destructurings" "x (Cons (Nothing) (Just x)). undefined"
    test "record literal"
      "{\n\
      \  x = xx. undefined,\n\
      \  y = yy,\n\
      \  z,\n\
      \  d = dd, e = ee\n\
      \  ,another,\n\
      \}"
    test "record literal one line no trailing comma" "{ a, b, c = undefined, d }"
    test "record literals nested" "{ a, b = { this }, c, d = {}}"
    test "record literal in function" "x y. { x, y }"
    test "match empty" "match { }"
    test "match simple"
      "f. match {\n\
      \  (Cons x xs) = f x $ xs\n\
      \  (Nil) = Nil\n\
      \}"
    test "match trailers"
      "match {\n\
      \  (Cons x xs) = f x $ xs\n\
      \  (Nil) = Nil\n\
      \} list"
    test "match no braces"
      "f. match\n\
      \  (Cons x xs) = f x $ xs\n\
      \  (Nil) = Nil\n"
    test "where clause in function"
      "x y. putStrLn $ first id $ second id $ (x, y)\n\
      \  where\n\
      \    first = f x.\n\
      \      (f $ fst x, snd x)\n\
      \    second = f x.\n\
      \      (fst x, f $ snd x)"
    test "multiline operator"
      "combinator $ `>>\n\
      \  putStrLn \"Who are you?\"\n\
      \  K $ getLine\n\
      \  name. putStrLn $\n\
      \    \"Hello, \" ++ name\n\
      \  K $ putStrLn \"Wow, you have a long name!\" <* guard $$ length name > 10"
