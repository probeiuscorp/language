module Main (main) where

import Compiler.Prelude
import Options.Applicative
import Compiler.ModuleTypes (ModuleIdentifier (LocalFileModule))
import Compiler.Modules (buildModules)
import Compiler.Interpret (interpretMainFile)
import System.Directory (canonicalizePath)

data Options = Options
  { optMainFile :: FilePath
  } deriving (Eq, Ord, Show)

parseOptions :: Parser Options
parseOptions = Options
  <$> argument str $: mconcat
    [ metavar placeholderFileName
    , help "Input file"
    ]
  -- <*> strOption $: mconcat
  --   [ long "out"
  --   , short 'o'
  --   , metavar placeholderFileName
  --   , help "Output file"
  --   ]
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
  mainIdentifier <- LocalFileModule <$> canonicalizePath $: optMainFile options
  sBuild <- buildModules mainIdentifier
  case sBuild of
    Left errs -> print errs
    Right build -> interpretMainFile build
