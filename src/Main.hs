import System.Console.ANSI
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO

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

main :: IO ()
main = do
    banner
    opts <- execParser optsParser
    putStrLn ""
    where
        -- Use "./lumi --x" ('lumi' is not a command)
        optsParser = info (helper <*> versionOption) fullDesc
        versionOption :: Parser (a -> a)
        versionOption = infoOption "Version 0.0.1" (long "version" <> help "Show current version")
        
        -- https://www.fpcomplete.com/haskell/library/optparse-applicative/
        -- programOptions :: Parser Opts
        -- programOptions =
        --     Opts <$> switch (long "global-flag" <> help "Set a global flag") <*>
            -- hsubparser lumiREPL
        -- lumiREPL ::  Mod CommandFields Command
        -- lumiREPL = 
        --     command
        --         "repl"
        --         (info "Lumi_REPL_Parser_Command" (progDesc "Lumi Interactive Compiler"))

        -- TODO: 
        -- lumiInterpreter: lumi <source file> -args
        -- lumiCompiler: lumi -c <source file> <output file>
        -- and corresponding commands of the 3 options

