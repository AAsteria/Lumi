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

main :: IO ()
main = do
    banner
    opts <- execParser optsParser
    repl [("x", SVal 0)]
    putStrLn ""
  where
    optsParser = info (helper <*> versionOption) fullDesc
    versionOption = infoOption "Version 0.0.1" (long "version" <> help "Show current version")

        -- https://www.fpcomplete.com/haskell/library/optparse-applicative/
        -- lumiInterpreter: lumi <source file> -args
        -- lumiCompiler: lumi -c <source file> <output file>
        -- and corresponding commands of the 3 options