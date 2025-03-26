{-# LANGUAGE MultiWayIf #-}

module Compiler.ParseInfix (parseInfix) where

import qualified Compiler.AST as AST
import Compiler.Linearize (Linear, Linearized, GLinearized(..))
import qualified Compiler.Zipper as Z
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (Bifunctor(first))
import Data.Char (isAlphaNum)
import Data.Function (on)

data Op = OpFn String AST.Infix | OpApplication
  deriving (Eq, Show)
data Operator = Operator Op Operand | StackDone
  deriving (Eq, Show)
data OperandF a = Operand a Operator
  deriving (Eq, Show)
-- | May not contain unhandled prefix functions. Operator stacks can only be built
-- off these kinds of stacks which do not have unhandled prefix functions.
type Operand = OperandF AST.Term
-- | A stack which may contain unhandled prefix function
type OperandAccum = OperandF StackValue
data StackValue
  = ValRegular AST.Term
  | ValPrefix (NE.NonEmpty AST.Term)
  deriving (Eq, Ord, Show)
data InfixStack = OperatorStack Operator | OperandStack OperandAccum
  deriving (Eq, Show)

expectRegularVal :: StackValue -> AST.Term
expectRegularVal (ValRegular term) = term
expectRegularVal _ = error "missing expression after prefix operator"
expectRegular :: OperandAccum -> Operand
expectRegular (Operand val stack) = Operand (expectRegularVal val) stack

type ParseOneTerm = Linear -> Maybe (AST.Term, Linear)
parseInfix :: ParseOneTerm -> Linear -> AST.Term
parseInfix pt z = let (zr, stack) = treeificateLinear pt (z, OperatorStack StackDone) in case stack of
  OperandStack operand -> collapseStep $ expectRegular operand
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
    addTerm term = case (state, (\op -> (op, opFixity op)) <$> isInfixOp term) of
      -- ADDING OPERANDS. Prefix operators are treated like regular values unless regular values appear before them.
      (OperatorStack stack, Nothing) -> OperandStack $ Operand (ValRegular term) stack
      (OperatorStack stack, Just (_, AST.FixityPrefix)) -> OperandStack $ Operand (ValPrefix $ NE.singleton term) stack

      -- ADDING OPERAND AFTER OPERAND
      -- Operand after regular operand, treat as application
      (OperandStack (Operand (ValRegular regularTerm) stackr), Nothing) -> OperandStack $ Operand (ValRegular term) $ Operator OpApplication $ Operand regularTerm stackr
      -- Operand after prefix operand, add to stack after applying prefix fn
      (OperandStack (Operand (ValPrefix fs) stack), Nothing) -> OperandStack $ Operand (ValRegular $ foldl (flip AST.TermApplication) term fs) stack
      -- Prefix after regular operand
      (OperandStack (Operand (ValRegular regularTerm) stackr), Just (_, AST.FixityPrefix)) -> OperandStack $ Operand (ValPrefix $ NE.singleton term) $ Operator OpApplication $ Operand regularTerm stackr
      -- Prefix after operand, add to prefix stack
      (OperandStack (Operand (ValPrefix fs) stack), Just (_, AST.FixityPrefix)) -> OperandStack $ Operand (ValPrefix $ term NE.<| fs) stack

      -- INFIX OPERATOR AFTER OPERAND
      (OperandStack stack, Just (ident, AST.FixityInfix inf)) -> addStack (OpFn ident inf) $ expectRegular stack
      -- Postfix operator: apply to top of stack
      (OperandStack stack, Just (_, AST.FixityPostfix)) -> let (Operand termE stackr) = stack in
        OperandStack $ Operand (ValRegular $ AST.TermApplication term $ expectRegularVal termE) stackr

      -- INFIX OPERATOR AFTER INFIX OPERATOR. Ignore to enable cleaner git diffs.
      (stack@(OperatorStack _), Just (_, AST.FixityInfix _)) -> stack
      (OperatorStack _, Just (_, AST.FixityPostfix)) -> error "missing expression before postfix operator"
      where
        addStack :: Op -> Operand -> InfixStack
        addStack op0 stack = let finishStack = OperatorStack . Operator op0 in
          case peekOperator stack of
            Just op | opPrecedence op > opPrecedence op0 ->
              finishStack $ collapseStack op stack
            _ -> finishStack stack

type Terms = [(AST.Term, Op)]
collapseStack :: Op -> Operand -> Operand
collapseStack op stack = uncurry Operand $ collapseWhile stack []
  where
    append = \case
      OpFn ident _ -> AST.TermApplication . AST.TermApplication (AST.TermIdentifier ident)
      OpApplication -> AST.TermApplication
    fold :: AST.Term -> Terms -> AST.Term
    fold term [] = term
    fold term ((t, op):xs) = case opAssociativity op of
      AST.LeftAssociative -> fold (append op term t) xs
      AST.RightAssociative -> append op term (fold t xs)
      AST.NonAssociative -> case xs of
        [] -> append op term t
        _ -> error "cannot mix non-associative infix operators"
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

opFixity :: String -> AST.Fixity
opFixity "¬" = AST.FixityPrefix
opFixity "~" = AST.FixityPrefix
opFixity "!" = AST.FixityPostfix
opFixity "?" = AST.FixityPostfix
opFixity ident = AST.FixityInfix $ AST.Infix (getOpPrecedence ident) (getOpAssociativity ident)

opPrecedence :: Op -> Double
opPrecedence (OpFn _ inf) = AST.infPrecedence inf
opPrecedence OpApplication = 10

opAssociativity :: Op -> AST.Associativity
opAssociativity (OpFn _ inf) = AST.infAssociativity inf
opAssociativity OpApplication = AST.LeftAssociative

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
