{-# LANGUAGE OverloadedStrings #-}

module Exercises.Tests
( tests
) where

import Core
import Study.Courses.CourseStore (courseStore)
import Study.Decks.DeckStore (deckStore)
import System.Random (mkStdGen)
import Control.Monad (forM_)
import Control.Applicative ((<$>))
import Test.Hspec
import Util (isTextWhitespace)
import qualified Data.Map as M
import qualified Data.Text as T

ensureNonWhitespace :: (Show c) => c -> T.Text -> IO ()
ensureNonWhitespace context text = shouldNotSatisfy context (const $ isTextWhitespace text)

validateExercise :: Exercise -> IO ()
validateExercise exercise =
    let context = exercise
    in case exercise of
        (SingleChoiceExercise title sentences correctAlternative incorrectAlternatives isFixedOrdering) -> do
            ensureNonWhitespace context title
            forM_ sentences (ensureNonWhitespace context . esText)
            ensureNonWhitespace context correctAlternative
            forM_ incorrectAlternatives (ensureNonWhitespace context)
        _ -> mempty

validateExerciseGenerator :: ExerciseGenerator -> IO ()
validateExerciseGenerator exerciseGenerator = do
    let exercises = exerciseGenerator . mkStdGen <$> [1..5000]
    forM_ exercises validateExercise
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
      it "All exercises are successfully generated, without throwing an exception, and look reasonable" $ do
        forM_ allExerciseGenerators validateExerciseGenerator

tests = exerciseTests
