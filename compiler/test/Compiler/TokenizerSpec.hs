module Compiler.TokenizerSpec (spec) where

import Test.Hspec
import Compiler.Tokenizer (tokenize, kind, content, Token(Token), TokenKind(..))

spec = describe "Compiler.Tokenizer" $ do
    it "export main:IO()\\t=of()\\n" $ do
        tokenize "export main:IO()\t=of()\n" `shouldBe` [
                Token { kind = LetterIdentifier, content = "export" },
                Token { kind = InlineWhitespace, content = " " },
                Token { kind = LetterIdentifier, content = "main" },
                Token { kind = SymbolIdentifier, content = ":" },
                Token { kind = LetterIdentifier, content = "IO" },
                Token { kind = SymbolIdentifier, content = "()" },
                Token { kind = InlineWhitespace, content = "\t" },
                Token { kind = SymbolIdentifier, content = "=" },
                Token { kind = LetterIdentifier, content = "of" },
                Token { kind = SymbolIdentifier, content = "()" },
                Token { kind = EOL, content = "\n" }
            ]
