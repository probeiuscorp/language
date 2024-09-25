module Compiler.Linearizer where

import Compiler.Tokenizer
import qualified Compiler.Zipper as Z

type Tokens = Z.Zipper Token

type Linearization = [Linearized]
data Linearized
  = LinFunction Linearization Linearization
  | LinBrackets Linearization
  | LinBraces Linearization
  | LinParens Linearization
  | LinTuple [Linearization]
  | LinTokens [Token]
  deriving (Eq, Show)

-- | Linearizing sorts out productions which require seeking. Mostly it is for
-- handling function expressions, but while it's here, it may as well handle
-- closing pairs too.
linearize :: Tokens -> [Linearized]
linearize = undefined
