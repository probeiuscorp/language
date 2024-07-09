module Compiler.TokenizerSpec (spec) where

import Test.Hspec
import Compiler.Tokenizer (tokenize, kind, content, Token(Token), TokenKind(..))

spec :: SpecWith ()
spec = describe "Compiler.Tokenizer" $ do
  it "export main:IO()\\t=of()\\n" $ do
    tokenize "export main:IO()\t=of()\n" `shouldBe`
      [ Token { kind = LetterIdentifier, content = "export" }
      , Token { kind = InlineWhitespace, content = " " }
      , Token { kind = LetterIdentifier, content = "main" }
      , Token { kind = SymbolIdentifier, content = ":" }
      , Token { kind = LetterIdentifier, content = "IO" }
      , Token { kind = SymbolIdentifier, content = "(" }
      , Token { kind = SymbolIdentifier, content = ")" }
      , Token { kind = InlineWhitespace, content = "\t" }
      , Token { kind = SymbolIdentifier, content = "=" }
      , Token { kind = LetterIdentifier, content = "of" }
      , Token { kind = SymbolIdentifier, content = "(" }
      , Token { kind = SymbolIdentifier, content = ")" }
      , Token { kind = EOL, content = "\n" }
      ]
  it "should tokenize parenthesis alone" $ do
    let symbol a = Token { kind = SymbolIdentifier, content = a }
    tokenize ")::())(" `shouldBe`
      [ symbol ")"
      , symbol "::"
      , symbol "("
      , symbol ")"
      , symbol ")"
      , symbol "("
      ]
  it "should tokenize math symbols alone" $ do
    tokenize "∀x. x + x==2x" `shouldBe`
      [ Token { kind = SymbolIdentifier, content = "∀" }
      , Token { kind = LetterIdentifier, content = "x" }
      , Token { kind = SymbolIdentifier, content = "." }
      , Token { kind = InlineWhitespace, content = " " }
      , Token { kind = LetterIdentifier, content = "x" }
      , Token { kind = InlineWhitespace, content = " " }
      , Token { kind = SymbolIdentifier, content = "+" }
      , Token { kind = InlineWhitespace, content = " " }
      , Token { kind = LetterIdentifier, content = "x" }
      , Token { kind = SymbolIdentifier, content = "==" }
      , Token { kind = NumberLiteral, content = "2" }
      , Token { kind = LetterIdentifier, content = "x" }
      ]
