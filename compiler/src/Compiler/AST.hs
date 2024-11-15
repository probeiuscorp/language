module Compiler.AST where

type ValidIdentifier = String

data Destructuring
  = DestructBind ValidIdentifier
  | DestructNominal (ValidIdentifier, [Destructuring])
  | DestructRecord [(ValidIdentifier, Maybe Destructuring)]
  deriving (Eq, Show)

-- | Terms are isomorphic with the source as-written (except for whitespace)
data Term
  = TermFunction [Destructuring] Term
  | TermApplication Term Term
  | TermIdentifier ValidIdentifier
  | TermRecord [(String, Maybe Term)]
  | TermTuple [Term]
  | TermList [Term]
  deriving (Eq, Show)
data TypeExpression = TypeExpression deriving (Eq, Show)

-- | Expressions are isomorphic with the compiled form
data Expression
  = ExFunction Destructuring Expression
  | ExApplication Expression Expression
  | ExIdentifier ValidIdentifier
  | ExRecord [(String, Expression)]
  deriving (Eq, Show)

data Associativity = LeftAssociative | RightAssociative deriving (Eq, Show)
data InfixDeclaration = InfixDeclaration
  { precedence :: Double
  , infixIdent :: ValidIdentifier
  , associativity :: Associativity
  } deriving (Eq, Show)

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
  | ExportDeclaration [(ValidIdentifier, Term)]
  | DataDeclaration ValidIdentifier [TypeExpression]
  | ValueDeclaration DeclarationModule (Maybe Term) (Maybe TypeExpression)
  | TypeDeclaration DeclarationModule TypeExpression
  deriving (Eq, Show)
