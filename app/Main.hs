module Main where

import Lambda
import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          case args of
            [f] -> do res <- run <$> readFile f
                      case res of
                        Left p  -> print p
                        Right s -> putStrLn s
            _   -> putStrLn "Wrong number of arguments"
