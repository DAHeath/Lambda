module Main where

import Lambda
import System.Environment (getArgs)

main :: IO ()
main = do res <- run . concat <$> getArgs
          case res of
            Left p  -> print p
            Right s -> putStrLn s
