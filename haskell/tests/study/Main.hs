module Main where

import Test.Hspec
import Study.Framework.Lojban.ExerciseGenerators.Tests as EG

main :: IO ()
main = hspec EG.tests
