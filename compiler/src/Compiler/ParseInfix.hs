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
  OperandStack operand -> case peekOperator operand of
    Just op -> case collapseStack (op, opPrecedence op) operand of
      Operand term StackDone -> term
      _ -> error "there was stuff still on the stack"
    Nothing -> error "how did we get here?"
  _ -> error "unclosed function"

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
              finishStack $ collapseStack (op, opPrecedence op) stack
            _ -> finishStack stack

collapseStack :: (Op, Int) -> Operand -> Operand
collapseStack (op, precedence) stack = let (terms, stackr) = collapseWhile stack in
  Operand (foldDirection fn terms) stackr
  where
    fn :: AST.Term -> AST.Term -> AST.Term
    fn = case op of
      OpFn ident -> flip $ AST.TermApplication . AST.TermApplication (AST.TermIdentifier ident)
      OpApplication -> flip AST.TermApplication
    foldDirection = case opAssociativity op of
      AST.LeftAssociative -> foldl1
      AST.RightAssociative -> foldr1
    collapseWhile :: Operand -> (NE.NonEmpty AST.Term, Operator)
    collapseWhile (Operand term n@(Operator nextOp xs)) = if
      -- Keep collapsing
      | op == nextOp -> first (term NE.<|) $ collapseWhile xs
      -- Collapse next group too
      | opPrecedence nextOp > precedence -> undefined
      -- Done
      | otherwise -> (pure term, n)
    collapseWhile (Operand term StackDone) = (pure term, StackDone)

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
opAssociativity _ = AST.RightAssociative
