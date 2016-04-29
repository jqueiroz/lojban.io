{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Server where

import Core
import DictionaryLoader (loadDictionary)
import Serializer (exerciseToJSON, validateExerciseAnswer)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Lessons.Grammar.Lesson as GrammarLesson
import System.Random (newStdGen, mkStdGen)
import qualified Text.Blaze as B
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

main :: IO ()
main = do
    dictionary <- loadDictionary
    simpleHTTP nullConf $ msum
        [ dir "static" $ serveDirectory EnableBrowsing [] "static"
        , dir "exercises" $ handleExercises dictionary
        , handleHome dictionary
        ]

-- Utility functions
internalStylesheet :: String -> H.Html
internalStylesheet src =
    H.link
      B.! A.href (H.stringValue $ "/static/style/" ++ src)
      B.! A.rel "stylesheet"

externalStylesheet :: String -> H.Html
externalStylesheet src =
    H.link
      B.! A.href (H.stringValue src)
      B.! A.rel "stylesheet"

internalScript :: String -> H.Html
internalScript src =
    H.script ""
      B.! A.type_ "text/javascript"
      B.! A.src (H.stringValue $ "/static/scripts/" ++ src)

externalScript :: String -> H.Html
externalScript src =
    H.script ""
      B.! A.type_ "text/javascript"
      B.! A.src (H.stringValue src)

-- Home page
handleHome :: Dictionary -> ServerPart Response
handleHome dictionary = ok $ toResponse $
    H.html $ do
      H.head $ do
        H.title (H.toHtml ("Lojto" :: T.Text))
        internalStylesheet "bootstrap.min.css"
        internalStylesheet "main.css"
        internalStylesheet "funkyradio.css"
        internalStylesheet "list-group-horizontal.css"
        internalStylesheet "exercise.css"
        externalStylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
        internalScript "jquery-2.1.4.min.js"
        internalScript "bootstrap.min.js"
          H.div ""
            B.! A.id "exercise-holder"

handleExercises :: Dictionary -> ServerPart Response
handleExercises dictionary = do
    let lesson = GrammarLesson.exercises2 dictionary
    gen <- liftIO $ newStdGen
    path $ \n -> let exercise = lesson (mkStdGen n) in msum
        [ dir "get" $ do
            ok $ toResponse (A.encode $ exerciseToJSON gen exercise)
        , dir "submit" $ do
            body <- getBody
            ok . toResponse . A.encode . A.object $ case validateExerciseAnswer exercise body of
                Nothing -> [("success", A.Bool False)]
                Just data' -> [("success", A.Bool True), ("data", data')]
        ]

getBody :: ServerPart BS.ByteString
getBody = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return ""

{-handleExercises :: (ToMessage a) => ServerPartT IO a-}
{-handleExercises = ok ("oi" :: T.Text)-}
