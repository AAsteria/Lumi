module Main where
import Parser
import AST
import Eval

import System.Console.ANSI
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO
import Prelude hiding (lookup)
import Control.Monad
import System.Console.Haskeline

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
repl :: Env -> IO ()
repl env = runInputT defaultSettings l
    where l = do p <- getInputLine "lumi> "
                    case p of
                        Nothing -> return ()
                        Just "quit" -> return ()
                        Just input -> do case parses input of
                                            Right exp -> outputStrLn (show $ eval exp env)
                                            Left msg -> outputStrLn (show msg)
                                        l

main :: IO ()
main = do
    banner
    repl emptyEnv -- TODO: Create lumi command for it
    opts <- execParser optsParser
    putStrLn ""
    where
        -- stack build, then use "./lumi --x" ('lumi' is not a command)
        optsParser = info (helper <*> versionOption) fullDesc
        versionOption :: Options.Applicative.Parser (a -> a)
        versionOption = infoOption "Version 0.0.1" (long "version" <> help "Show current version")
        
        -- https://www.fpcomplete.com/haskell/library/optparse-applicative/
        -- lumiInterpreter: lumi <source file> -args
        -- lumiCompiler: lumi -c <source file> <output file>
        -- and corresponding commands of the 3 options

