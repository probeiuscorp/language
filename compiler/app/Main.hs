module Main (main) where

import Options.Applicative
import LLVM.Module
import qualified Data.ByteString as BS
import Compiler.Modules (parseModule, verifyModuleBuildable)
import Compiler.IR (mkMainModule)
import LLVM.Context (withContext)

($$) = ($)
infixr 6 $$

data Options = Options
  { optMainFile :: FilePath
  , optOutFile :: FilePath
  } deriving (Eq, Ord, Show)

parseOptions :: Parser Options
parseOptions = Options
  <$> argument str $$ mconcat
    [ metavar placeholderFileName
    , help "Input file"
    ]
  <*> strOption $$ mconcat
    [ long "out"
    , short 'o'
    , metavar placeholderFileName
    , help "Output file"
    ]
  where
    placeholderFileName = "<filename>"

opts :: ParserInfo Options
opts = info (helper <*> parseOptions) $ mconcat
  [ fullDesc
  , progDesc "Compiler for Tilly"
  ]

main :: IO ()
main = do
  options <- execParser opts
  src <- readFile $ optMainFile options
  let mainModule = verifyModuleBuildable $ parseModule src
  case mainModule of
    Left errs -> print errs
    Right llvmModule -> do
      llvm <- withContext $ \context ->
        withModuleFromAST context (mkMainModule llvmModule) moduleLLVMAssembly
      BS.writeFile (optOutFile options) llvm
