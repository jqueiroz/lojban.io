module Main where

import Test.Hspec
import Courses.Util.ExerciseGenerators.Tests as EG

main :: IO ()
main = hspec EG.tests
