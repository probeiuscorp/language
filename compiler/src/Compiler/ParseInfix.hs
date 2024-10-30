{-# LANGUAGE MultiWayIf #-}

module Compiler.ParseInfix (parseInfix) where

import qualified Compiler.AST as AST
import Compiler.Linearizer (Linear, Linearized, GLinearized(..))
import Compiler.Tokenizer
import qualified Compiler.Zipper as Z
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (Bifunctor(first))
import Data.Char (isAlphaNum)

data Op = OpFn String | OpApplication
  deriving (Eq, Show)
data Operator = Operator Op Operand | StackDone
  deriving (Eq, Show)
data Operand = Operand AST.Term Operator
  deriving (Eq, Show)
data InfixStack = OperatorStack Operator | OperandStack Operand
  deriving (Eq, Show)

parseInfix :: Linear -> AST.Term
parseInfix z = let (zr, stack) = treeificateLinear (z, OperatorStack StackDone) in case stack of
  OperandStack operand -> collapseStep operand
  _ -> error "unclosed function"
  where
    collapseStep :: Operand -> AST.Term
    collapseStep operand = case peekOperator operand of
      Just op -> case collapseStack op operand of
        Operand term StackDone -> term
        operand' -> collapseStep operand'
      Nothing -> error "how did we get here?"

type ParseState = (Linear, InfixStack)
treeificateLinear :: ParseState -> ParseState
treeificateLinear s@(z, state) = maybe s (\(term, zr) -> treeificateLinear (zr, addTerm term)) $ parseOneTerm z
  where
    addTerm :: AST.Term -> InfixStack
    addTerm term = case (state, isInfixOp term) of
      -- Infix operator after infix operator, not legal
      (OperatorStack stack, Just ident) -> error $ "Unexpected infix operator " ++ ident
      -- Operand after infix operator
      (OperatorStack stack, Nothing)    -> OperandStack (Operand term stack)
      -- Infix operator after operand
      (OperandStack stack, Just ident)  -> addStack ident stack
      -- Operand after operand, treat as application
      (OperandStack stack, Nothing)     -> OperandStack (Operand term $ Operator OpApplication stack)
      where
        addStack :: String -> Operand -> InfixStack
        addStack ident stack = let
          finishStack stack = OperatorStack (Operator (OpFn ident) stack)
          in case peekOperator stack of
            Just op | opPrecedence op > opPrecedence (OpFn ident) ->
              finishStack $ collapseStack op stack
            _ -> finishStack stack

collapseStack :: Op -> Operand -> Operand
collapseStack op stack = uncurry Operand $ collapseWhile stack []
  where
    precedence = opPrecedence op
    fold term terms = (case opAssociativity op of
      AST.LeftAssociative -> foldl1
      AST.RightAssociative -> foldr1
      ) (case op of
        OpFn ident -> AST.TermApplication . AST.TermApplication (AST.TermIdentifier ident)
        OpApplication -> AST.TermApplication
        ) (term:terms)
    collapseWhile :: Operand -> [AST.Term] -> (AST.Term, Operator)
    collapseWhile (Operand term n@(Operator nextOp xs)) terms = if
      -- Keep collapsing
      | op == nextOp -> collapseWhile xs $ term:terms
      -- Collapse next group too
      | opPrecedence nextOp > precedence -> collapseWhile (collapseStack nextOp (Operand term n)) terms
      -- Done
      | otherwise -> (fold term terms, n)
    collapseWhile (Operand term StackDone) terms = (fold term terms, StackDone)

peekOperator :: Operand -> Maybe Op
peekOperator (Operand _ (Operator op _)) = Just op
peekOperator _ = Nothing

isInfixOp :: AST.Term -> Maybe String
isInfixOp (AST.TermIdentifier ident) | not $ all isAlphaNum ident = Just ident
isInfixOp _ = Nothing

parseOneTerm :: Linear -> Maybe (AST.Term, Linear)
parseOneTerm z = Z.right z >>= (\(term, zr) -> case term of
  (LinToken t) | isWhitespace t -> parseOneTerm zr
  (LinToken t) | kind t == LetterIdentifier || kind t == SymbolIdentifier -> Just (AST.TermIdentifier $ content t, zr)
  l -> error $ "term not supported yet: " ++ show l
  )

opPrecedence :: Op -> Int
opPrecedence OpApplication = 10
opPrecedence (OpFn "$") = 1
opPrecedence (OpFn "-") = 4
opPrecedence _ = 6

opAssociativity :: Op -> AST.Associativity
opAssociativity OpApplication = AST.LeftAssociative
opAssociativity (OpFn "$") = AST.RightAssociative
opAssociativity (OpFn "-") = AST.LeftAssociative
opAssociativity (OpFn "*") = AST.LeftAssociative
opAssociativity (OpFn "/") = AST.LeftAssociative
opAssociativity _ = AST.RightAssociative
