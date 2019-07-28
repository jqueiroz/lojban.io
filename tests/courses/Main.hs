module Main where

import Test.Hspec
import Courses.Framework.ExerciseGenerators.Tests as EG

main :: IO ()
main = hspec EG.tests
