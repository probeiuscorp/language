module Compiler.LinearizeSpec (spec) where

import Test.Hspec
import qualified Compiler.Zipper as Z
import Compiler.Linearize
import Compiler.Tokenize (tokenize, content)
import Compiler.SnapshotTesting (snapshot)

prettyPrintLinearization :: [Linearized] -> String
prettyPrintLinearization l = "[\n" ++ go "" l ++ "]"
  where
    go :: String -> Linearization -> String
    go indent lbody = lbody >>= prettyPrintLinearized ("  " ++ indent)
    prettyPrintLinearized :: String -> Linearized -> String
    prettyPrintLinearized indent t@(LinToken _) = indent ++ (show . fmap content) t ++ ",\n"
    prettyPrintLinearized indent (LinFunction lparams lbody) = indent ++ "LinFunction" ++ showLinearization indent lparams ++ showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinParens lbody)   = indent ++ "LinParens" ++   showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinBraces lbody)   = indent ++ "LinBraces" ++   showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinBrackets lbody) = indent ++ "LinBrackets" ++ showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinWhere lbody lclauses) = indent ++ "LinWhere" ++ showLinearization indent lbody ++ (if null lclauses then " []" else showLinearization indent =<< lclauses) ++ ",\n"
    prettyPrintLinearized indent (LinMultilineOperator operator lclauses) = indent ++ "LinMultilineOperator(" ++ operator ++ ")" ++ (if null lclauses then " []" else showLinearization indent =<< lclauses) ++ ",\n"
    prettyPrintLinearized indent (LinError (LinUnmatchedClosingPair t) _) = indent ++ "LinUnmatchedClosingPair " ++ show (content t) ++ ",\n"
    showLinearization :: String -> Linearization -> String
    showLinearization indent lbody = " [\n" ++ go indent lbody ++ indent ++ "]"

spec :: SpecWith ()
spec = describe "linearize" $ do
  let prettyParseLinearization = prettyPrintLinearization . linearize . Z.start . tokenize
  let test = snapshot "linearize/" prettyParseLinearization
  test "parens" "a (b c d) e"
  test "unclosed parens" "a b (c d e"
  test "nested parens" "a ((b c) d) e"
  test "braces" "a {b c d} e"
  test "unclosed braces"
    "{\n\
    \x = 10,\n\
    \y = 20,\n"
  test "mixed unclosed"
    "x = ({ w: 5 }) & ({\n\
    \x = (z. z + 10),\n\
    \y = 20,\n"
  test "unmatched pair" "f x y)"
  test "unmatched pair nested" "(f x y))"
  test "unmatched pair nested distinct" "[1, 2, 3, 4}]"
  test "nested braces" "a {{b c} d} e"
  test "mixed nestings" "(a {b c}) d (e)"
  test "functions" "x y. x"
  test "functions with parentheses" "x (Cons (Just x) xs). undefined"
  test "function in parentheses" "(x y. x)"
  test "function with comma in parenthesis in body" "(x y. x (x y, y) z)"
  test "functions in tuples" "(x y. x, x y. y)"
  test "functions in tuples with trailing comma" "(x y. x, x y. y,)"
  test "functions in brace rhs"
    "{\n\
    \  a = x y. x,\n\
    \  b = x y. y,\n\
    \  c = undefined,\n\
    \}"
  test "functions in brace rhs with trailing comma"
    "{\n\
    \  a = x y. x,\n\
    \  b = undefined,\n\
    \  c = x y. y,\n\
    \}"
  test "functions in brace rhs no trailing comma"
    "{\n\
    \  a = x y. x,\n\
    \  b = undefined,\n\
    \  c = x y. y\n\
    \}"
  test "where clause simple"
    "main = putStrLn $ first id $ second id $ (3, 4)\n\
    \  where\n\
    \    first = f x.\n\
    \      (f $ fst x, snd x)\n\
    \    second = f x.\n\
    \      (fst x, f $ snd x)"
  test "where clause blank lines"
    "main = putStrLn $ first id $ second id $ (3, 4)\n\
    \  where\n\
    \  \n\
    \    first = f x.\n\
    \      (f $ fst x, snd x)\n\
    \  \n\
    \    second = f x.\n\
    \      (fst x, f $ snd x)"
  test "where clause in function"
    "main = x y. putStrLn $ first id $ second id $ (3, 4)\n\
    \  where\n\
    \    first = f x.\n\
    \      (f $ fst x, snd x)\n\
    \    second = f x.\n\
    \      (fst x, f $ snd x)"
  test "where clause no clauses"
    "main = putStrLn\n\
    \  where"
  test "multiline operator"
    "combinator $ `>>\n\
    \  putStrLn \"Who are you?\"\n\
    \  K $ getLine\n\
    \  name. putStrLn $\n\
    \    \"Hello, \" ++ name\n\
    \  K $ putStrLn \"Wow, you have a long name!\" <* guard $$ length name > 10"
