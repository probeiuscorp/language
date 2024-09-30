module Compiler.AST where

type ValidIdentifier = String

data Destructuring
  = DestructBind ValidIdentifier
  | DestructCons (ValidIdentifier, [Destructuring])
  | DestructRecord [(ValidIdentifier, Destructuring)]
  deriving (Eq, Show)
data Expression
  = ExFunction [Destructuring] Expression
  | ExApplication Expression Expression
  | ExIdentifier ValidIdentifier
  | ExStringLiteral String
  | ExNumberLiteral Double
  | ExRecord [(String, Expression)]
  deriving (Eq, Show)
data TypeExpression = TypeExpression deriving (Eq, Show)

data Associativity = LeftAssociative | RightAssociative deriving (Eq, Show)
data InfixDeclaration = InfixDeclaration
  { precedence :: Double
  , infixIdent :: ValidIdentifier
  , associativity :: Associativity
  } deriving (Eq, Show)

data ImportListing
  = ImportAll
  | ImportOnly [(ValidIdentifier, Expression)]
  | ImportHiding [(ValidIdentifier, Expression)]
  | ImportAs ValidIdentifier
  deriving (Eq, Show)

data DeclarationModule = DeclarationModule
  { identifier :: ValidIdentifier
  , isExported :: Bool
  } deriving (Eq, Show)
data TopLevelDeclaration
  = ImportDeclaration String ImportListing
  | ExportDeclaration [(ValidIdentifier, Expression)]
  | DataDeclaration ValidIdentifier [TypeExpression]
  | ValueDeclaration DeclarationModule (Maybe Expression) (Maybe TypeExpression)
  | TypeDeclaration DeclarationModule TypeExpression
  deriving (Eq, Show)
