module Compiler.AST where

type ValidIdentifier = String
type PropertyKey = String

data Abstraction = Abstraction
  { binding :: PropertyKey
  , constraint :: Maybe TypeExpression
  , body :: Expression
  } deriving (Eq, Show)
data CallArgs a = CallArgs
  { target :: a
  , argument :: a
  } deriving (Eq, Show)
type ExpressionCallArgs = CallArgs Expression
type TypeExpressionCallArgs = CallArgs TypeExpression
data Expression
  = Function Abstraction
  | Application ExpressionCallArgs
  | Identifier ValidIdentifier
  | StringLiteral String
  | NumberLiteral Double
  | Record [Abstraction]
  deriving (Eq, Show)

data TypeExpression
  = TypeConstructor Abstraction
  | TypeApplication TypeExpressionCallArgs
  | TypeIdentifier ValidIdentifier
  | TypeofExpression Expression
  deriving (Eq, Show)

data Associativity = LeftAssociative | RightAssociative deriving (Eq, Show)
data InfixDeclaration = InfixDeclaration
  { precedence :: Double
  , infixIdent :: ValidIdentifier
  , associativity :: Associativity
  } deriving (Eq, Show)

data DestructuringMember = DestructuringMember
  { destructIdent :: ValidIdentifier
  , destructBinding :: ValidIdentifier
  } deriving (Eq, Show)
type Destructuring = [DestructuringMember]
data ImportListing
  = ImportAll
  | ImportOnly Destructuring
  | ImportHiding Destructuring
  | ImportAs ValidIdentifier
  deriving (Eq, Show)

data DeclarationModule = DeclarationModule
  { identifier :: ValidIdentifier
  , isExported :: Bool
  } deriving (Eq, Show)
data TopLevelDeclaration
  = ImportDeclaration String ImportListing
  | ExportDeclaration Destructuring
  | DataDeclaration String [TypeExpression]
  | ValueDeclaration DeclarationModule (Maybe Expression) (Maybe TypeExpression)
  | TypeDeclaration DeclarationModule TypeExpression
  deriving (Eq, Show)
