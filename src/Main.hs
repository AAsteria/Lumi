import System.Console.ANSI
import System.IO

banner :: IO ()
banner = do
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "Lumi Programming Language"
    putStrLn "Version 0.0.1"
    putStrLn ""

help :: IO ()
help = do
    setSGR [SetColor Foreground Dull Green]
    putStrLn "-------------- Manual --------------"
    putStrLn " 1. Run as Interactive:"
    putStrLn " 'lumi'"
    putStrLn " 2. Run as Interpreter:"
    putStrLn " 'lumi <source file> -args'"
    putStrLn " 3. Run as Compiler:"
    putStrLn " 'lumi -c <source file> <output file>'"
    putStrLn " Help Information:"
    putStrLn " 'lumi -?' or 'lumi -help'"
    putStrLn ""

main :: IO ()
main = do
    banner
    help
    putStrLn ""
    -- TODO: run interactively