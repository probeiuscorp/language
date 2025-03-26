module Compiler.AST where
import Compiler.Tokenize (NumberContents)
import qualified Data.Set as Set

type ValidIdentifier = String

data Destructuring
  = DestructBind ValidIdentifier
  | DestructAs ValidIdentifier Destructuring
  | DestructNominal ValidIdentifier [Destructuring]
  | DestructRecord [(ValidIdentifier, Maybe Destructuring)]
  deriving (Eq, Show, Ord)

-- | Terms are isomorphic with the source as-written (except for whitespace)
data Term
  = TermFunction [Destructuring] Term
  | TermApplication Term Term
  | TermIdentifier ValidIdentifier
  | TermWhere Term [(Destructuring, Term)]
  | TermMultilineOperator Term [Term]
  | TermNumberLiteral NumberContents
  | TermStringLiteral String
  | TermRecord [(String, Maybe Term)]
  | TermTuple [Maybe Term]
  | TermList [Maybe Term]
  | TermMatch [([Destructuring], Term)]
  deriving (Eq, Show, Ord)

type VarSet = Set.Set ValidIdentifier
-- | Expressions are isomorphic with the compiled form
data Expression
  = ExprFunction VarSet Destructuring Expression
  | ExprApplication Expression Expression
  | ExprIdentifier ValidIdentifier
  | ExprIntegral Int
  | ExprDouble Double
  | ExprRecord [(String, Expression)]
  | ExprTuple [Expression]
  | ExprList [Expression]
  | ExprMatch [(Destructuring, Expression)]
  deriving (Eq, Show)

data ImportListing
  = ImportAll
  | ImportOnly Destructuring
  | ImportHiding Destructuring
  | ImportAs ValidIdentifier
  deriving (Eq, Show, Ord)
data DeclarationModule = DeclarationModule
  { identifier :: ValidIdentifier
  , isExported :: Bool
  } deriving (Eq, Show, Ord)
data Associativity = NonAssociative | LeftAssociative | RightAssociative deriving (Eq, Show, Ord)
data Infix = Infix { infPrecedence :: Double, infAssociativity :: Associativity } deriving (Eq, Ord, Show)
data Fixity = FixityInfix Infix | FixityPrefix | FixityPostfix deriving (Eq, Ord, Show)
data TopLevelDeclaration
  = ImportDeclaration String ImportListing
  | ExportDeclaration [(ValidIdentifier, Term)]
  | DataDeclaration DeclarationModule (Maybe Term)
  | ValueDeclaration DeclarationModule (Maybe Term) (Maybe Term)
  | TypeDeclaration DeclarationModule Term
  | InfixDeclaration ValidIdentifier Fixity
  deriving (Eq, Show, Ord)

data ParseError
  = ErrUnknownIdentifier String
  deriving (Eq, Show, Ord)
