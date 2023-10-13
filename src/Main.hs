module Main where

import Parser
import AST
import Eval

import System.Console.ANSI
    ( setSGR,
      Color(Green),
      ColorIntensity(Dull, Vivid),
      ConsoleLayer(Foreground),
      SGR(SetColor) )
import Options.Applicative as OA
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
import System.Directory.Internal.Prelude
import System.Exit

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
repl :: (Fractional a, Ord a, Show a, Floating a) => Env a -> InputT IO ()
repl env = do
    input <- getInputLine "Lumi> "
    case input of
        Nothing -> return ()  -- Exit on Ctrl+D or EOF
        Just "exit" -> return ()
        Just "quit" -> return ()
        Just "" -> repl env
        Just inputStr -> do
            case parse stmt "<stdin>" inputStr of
                Right exp -> do
                    (output, nuenv) <- liftIO $ execStmt env exp
                    case output of
                        Just foo -> outputStrLn foo
                        Nothing -> outputStrLn "Done."
                    repl nuenv
                Left err -> do
                    outputStrLn (errorBundlePretty err)
                    repl env

-- Lumi Interpreter
interpretFile :: (Fractional a, Ord a, Show a, Floating a) => FilePath -> Env a -> IO ()
interpretFile filePath env = do
    input <- catch (readFile filePath) handler
    case parse mmvp filePath input of
        Left err -> print err
        Right exp -> print $ eval exp env
  where
    handler :: IOError -> IO String
    handler e
      | isDoesNotExistError e = do
          putStrLn $ "Error: file not found: " ++ filePath
          exitWith $ ExitFailure 1
      | otherwise = ioError e

main :: IO ()
main = do
    banner
    opts <- execParser optsParser
    case opts of
        LumiInterpreter source args -> interpretFile source []
        LumiCompiler source output -> lumiCompiler source output
        Repl _ -> runInputT defaultSettings $ repl []
    putStrLn ""
  where
    optsParser = info (helper <*> versionOption <*> commandParser <|> pure (Repl [])) fullDesc
    versionOption = infoOption "Version 0.0.1" (long "version" <> help "Show current version")

data LumiOpts
    = LumiInterpreter FilePath [String]
    | LumiCompiler FilePath FilePath
    | Repl [(String, SExp ())]

lumiInterpreter :: FilePath -> [String] -> IO ()
lumiInterpreter source args = do
    putStrLn $ "Compiling " ++ source ++ " with args " ++ show args ++ "..."

lumiCompiler :: FilePath -> FilePath -> IO ()
lumiCompiler source output = do
    let output' = if takeExtension output == ".exe" then output else output <> "exe"
    putStrLn $ "Compiling " ++ source ++ " to " ++ output'
    system $ "ghc " ++ source ++ " -o " ++ output'
    return ()

commandParser :: OA.Parser LumiOpts
commandParser =
    hsubparser
        ( command "interp" (info lumiInterpreterParser (progDesc "Interpret Lumi source file"))
            <> command "compile" (info lumiCompilerParser (progDesc "Compile Lumi source file"))
        )

lumiInterpreterParser :: OA.Parser LumiOpts
lumiInterpreterParser =
    LumiInterpreter <$> argument OA.str (metavar "SOURCE") <*> OA.many (strOption (long "arg" <> metavar "ARG"))

lumiCompilerParser :: OA.Parser LumiOpts
lumiCompilerParser =
    LumiCompiler <$> argument OA.str (metavar "SOURCE") <*> argument OA.str (metavar "OUTPUT")
