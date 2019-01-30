--
-- CS 430 Spring 2019 P4 (Haskell 2)
--

module Main where
import Control.Monad
import System.IO
import Defs
import Parser
import P4

-- main read/eval/print loop
repl :: IO()
repl = do
  input <- (putStr ">>> " >> hFlush stdout >> getLine)
  unless (input == "q") $
    let expr = parse input in do
      putStrLn $ "Expression:  " ++ input
      putStrLn $ "Evaluation:  " ++ (show $ eval expr)
      putStrLn $ "Count Ops:   " ++ (show $ countOps expr)
      putStrLn $ "Height:      " ++ (show $ height expr)
      putStrLn $ "Postfix:     " ++ (show $ postfix expr)
      putStrLn $ "Unique Ints: " ++ (show $ uniqInts expr)
      >> repl

-- program entry point
main :: IO()
main = do
  putStrLn "Expression tree calculator"
  putStrLn "\"q\" to quit."
  repl

