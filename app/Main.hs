module Main where

import Protolude (IO, putStrLn, ($))
import Lib (sayHello)

main :: IO ()
main = do
  putStrLn $ sayHello "World!"
