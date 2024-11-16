{-# LANGUAGE MultiWayIf #-}

module Compiler.ParseInfix (parseInfix) where

import qualified Compiler.AST as AST
import Compiler.Linearize (Linear, Linearized, GLinearized(..))
import qualified Compiler.Zipper as Z
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (Bifunctor(first))
import Data.Char (isAlphaNum)
import Data.Function (on)

data Op = OpFn String | OpApplication
  deriving (Eq, Show)
data Operator = Operator Op Operand | StackDone
  deriving (Eq, Show)
data Operand = Operand AST.Term Operator
  deriving (Eq, Show)
data InfixStack = OperatorStack Operator | OperandStack Operand
  deriving (Eq, Show)

type ParseOneTerm = Linear -> Maybe (AST.Term, Linear)
parseInfix :: ParseOneTerm -> Linear -> AST.Term
parseInfix pt z = let (zr, stack) = treeificateLinear pt (z, OperatorStack StackDone) in case stack of
  OperandStack operand -> collapseStep operand
  _ -> error "unclosed function"
  where
    collapseStep :: Operand -> AST.Term
    collapseStep (Operand term StackDone) = term
    collapseStep operand@(Operand term (Operator op _)) = collapseStep $ collapseStack op operand

type ParseState = (Linear, InfixStack)
treeificateLinear :: ParseOneTerm -> ParseState -> ParseState
treeificateLinear pt s@(z, state) = maybe s (\(term, zr) -> treeificateLinear pt (zr, addTerm term)) $ pt z
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

type Terms = [(AST.Term, Op)]
collapseStack :: Op -> Operand -> Operand
collapseStack op stack = uncurry Operand $ collapseWhile stack []
  where
    append op = case op of
      OpFn ident -> AST.TermApplication . AST.TermApplication (AST.TermIdentifier ident)
      OpApplication -> AST.TermApplication
    fold :: AST.Term -> Terms -> AST.Term
    fold term [] = term
    fold term ((t, op):xs) = case opAssociativity op of
      AST.LeftAssociative -> fold (append op term t) xs
      AST.RightAssociative -> append op term (fold t xs)
    collapseWhile :: Operand -> Terms -> (AST.Term, Operator)
    collapseWhile (Operand term n@(Operator nextOp xs)) terms = if
      -- Keep collapsing
      | ord == EQ -> if ((==) `on` opAssociativity) op nextOp
        then collapseWhile xs $ (term, nextOp):terms
        else error "cannot mix operations with different associativity and of same precedence"
      -- Collapse next group too
      | ord == GT -> collapseWhile (collapseStack nextOp (Operand term n)) terms
      -- Done
      | otherwise -> (fold term terms, n)
      where ord = (compare `on` opPrecedence) nextOp op
    collapseWhile (Operand term StackDone) terms = (fold term terms, StackDone)

peekOperator :: Operand -> Maybe Op
peekOperator (Operand _ (Operator op _)) = Just op
peekOperator _ = Nothing

isInfixOp :: AST.Term -> Maybe String
isInfixOp (AST.TermIdentifier ident) | not $ all isAlphaNum ident = Just ident
isInfixOp _ = Nothing

opPrecedence :: Op -> Int
opPrecedence OpApplication = 10
opPrecedence (OpFn "$") = 1
opPrecedence (OpFn "-") = 4
opPrecedence (OpFn "^") = 8
opPrecedence (OpFn "^^") = 8
opPrecedence _ = 6

opAssociativity :: Op -> AST.Associativity
opAssociativity OpApplication = AST.LeftAssociative
opAssociativity (OpFn "$") = AST.RightAssociative
opAssociativity (OpFn "-") = AST.LeftAssociative
opAssociativity (OpFn "*") = AST.LeftAssociative
opAssociativity (OpFn "/") = AST.LeftAssociative
opAssociativity _ = AST.RightAssociative
