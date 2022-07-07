module Main where

import qualified ProtoFmt (runFormatting)
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["--help"] = printUsage
parseArgs [f] = format f (readFile f)
parseArgs _ = printUsage

printUsage :: IO ()
printUsage = do
  n <- getProgName
  _ <- putStrLn $ "Usage: " ++ n ++ " " ++ "<file.proto>"
  exitSuccess

format :: String -> IO String -> IO ()
format s i = do
  c <- i
  case ProtoFmt.runFormatting s c of
    Left e -> putStr e >> exitFailure
    Right t -> putStr t >> exitSuccess
