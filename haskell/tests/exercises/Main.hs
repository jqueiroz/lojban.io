module Main where

import Test.Hspec
import Exercises.Tests as EG

main :: IO ()
main = hspec EG.tests
