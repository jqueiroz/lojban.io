{-# LANGUAGE OverloadedStrings #-}

module Exercises.Tests
( tests
) where

import Core
import Courses.CourseStore (courseStore)
import Decks.DeckStore (deckStore)
import System.Random (mkStdGen)
import Control.Monad (forM_)
import Control.Applicative ((<$>))
import Test.Hspec
import qualified Data.Map as M
import qualified Data.Text as T

validateExerciseGenerator :: ExerciseGenerator -> IO ()
validateExerciseGenerator exerciseGenerator = do
    let exercises = exerciseGenerator . mkStdGen <$> [1..1000]
    seq (length . show $ exercises) $ return ()

exerciseTests :: Spec
exerciseTests = do
    let allCourses = M.elems $ courseStoreCourses courseStore
    let allLessons = concat $ courseLessons <$> allCourses
    let allLessonExerciseGenerators = lessonExercises <$> allLessons
    let allDecks = M.elems $ deckStoreDecks deckStore
    let allDeckCards = concat $ deckCards <$> allDecks
    let allCardExerciseGenerators = cardExercises <$> allDeckCards
    let allExerciseGenerators = allLessonExerciseGenerators ++ allCardExerciseGenerators
    describe "All exercises are correct" $ do
      it "All exercises are successfully generated, without throwing an exception" $ do
        forM_ allExerciseGenerators validateExerciseGenerator

tests = exerciseTests
