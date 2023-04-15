module Main where
import Parser
import AST
import Eval

import System.Console.ANSI
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO
import Prelude hiding (lookup)
import Text.Megaparsec
import Control.Monad
import System.Console.Haskeline
import Language.Haskell.TH.PprLib (space)
import Text.Megaparsec.Byte (space1)
import Control.Monad.State
import qualified Data.Functor
import System.FilePath
import System.Process
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Except (runExceptT, liftIO)
import System.Directory (doesFileExist)
import qualified Data.Text.IO as TIO
import Data.Text as T

data Opts = Opts
    { optFlag :: !Bool
    , optVal :: !String
    }

banner :: IO ()
banner = do
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "Lumi Programming Language"
    setSGR [SetColor Foreground Dull Green]
    putStrLn ""

-- Lumi Repl
repl :: (Fractional a, Ord a, Show a, Floating a) => Env a -> IO ()
repl env = runInputT defaultSettings l
  where l = do p <- getInputLine "lumi> "
               case p of
                    Nothing -> pure ()
                    Just "exit" -> pure ()
                    Just "quit" -> pure ()
                    Just "" -> l
                    Just input -> do case parse mmvp "<stdin>" input of -- mmvp and stmt
                                       Right exp -> outputStrLn (show $ eval exp env)
                                       Left err -> outputStrLn (show err)
                                     l

-- Lumi Interpreter
interpretFile :: (Fractional a, Ord a, Show a, Floating a) => FilePath -> Env a -> IO ()
interpretFile filePath env = do
  input <- readFile filePath
  case parse mmvp filePath input of
    Left err -> print err
    Right exp -> print $ eval exp env

main :: IO ()
main = do
  banner
  opts <- execParser optsParser
  case opts of
    LumiInterpreter source args -> interpretFile source []
    LumiCompiler source output -> lumiCompiler source output
    Repl _ -> repl []
  putStrLn ""
  where
    optsParser = info (helper <*> versionOption <*> commandParser) fullDesc
    versionOption = infoOption "Version 0.0.1" (long "version" <> help "Show current version")

data LumiOpts
  = LumiInterpreter FilePath [String]
  | LumiCompiler FilePath FilePath
  | Repl [(String, SExp ())]

lumiInterpreter :: FilePath -> [String] -> IO ()
lumiInterpreter source args = do
  putStrLn $ "Compiling " ++ source ++ " with args " ++ show args

lumiCompiler :: FilePath -> FilePath -> IO ()
lumiCompiler source output = do
  let output' = if takeExtension output == ".exe" then output else output <> "exe"
  putStrLn $ "Compiling " ++ source ++ " to " ++ output'
  system $ "ghc " ++ source ++ " -o " ++ output'
  return ()

commandParser :: Options.Applicative.Parser LumiOpts
commandParser =
  hsubparser
    ( command "interpret" (info lumiInterpreterParser (progDesc "Interpret Lumi source file"))
        <> command "compile" (info lumiCompilerParser (progDesc "Compile Lumi source file"))
        <> command "repl" (info replParser (progDesc "Start Lumi REPL"))
    )

lumiInterpreterParser :: Options.Applicative.Parser LumiOpts
lumiInterpreterParser =
  LumiInterpreter <$> argument Options.Applicative.str (metavar "SOURCE") <*> Options.Applicative.many (strOption (long "arg" <> metavar "ARG"))

lumiCompilerParser :: Options.Applicative.Parser LumiOpts
lumiCompilerParser =
  LumiCompiler <$> argument Options.Applicative.str (metavar "SOURCE") <*> argument Options.Applicative.str (metavar "OUTPUT")

replParser :: Options.Applicative.Parser LumiOpts
replParser =
  Repl <$> Options.Applicative.many ((,) <$> argument Options.Applicative.str (metavar "VAR") <*> (SVal () <$ pure ()))
