module Compiler.TestOperators (testOperators) where

import qualified Compiler.AST as AST

testOperators :: AST.AboutOperators
testOperators "¬" = AST.FixityPrefix
testOperators "~" = AST.FixityPrefix
testOperators "!" = AST.FixityPostfix
testOperators "?" = AST.FixityPostfix
testOperators ident = AST.FixityInfix $ AST.Infix (getOpPrecedence ident) (getOpAssociativity ident)

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
