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

handleHome :: Dictionary -> ServerPart Response
handleHome dictionary = ok $ toResponse $
    H.html $ do
      H.head $ do
        H.title (H.toHtml ("Lojto" :: T.Text))
        H.link
          B.! A.href "/static/style/bootstrap.min.css"
          B.! A.rel "stylesheet"
        H.link
          B.! A.href "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
          B.! A.rel "stylesheet"
        H.link
          B.! A.href "/static/style/funkyradio.css"
          B.! A.rel "stylesheet"
        H.link
          B.! A.href "/static/style/list-group-horizontal.css"
          B.! A.rel "stylesheet"
        H.link
          B.! A.href "/static/style/main.css"
          B.! A.rel "stylesheet"
        H.link
          B.! A.href "/static/style/exercise.css"
          B.! A.rel "stylesheet"
        H.script ""
          B.! A.type_ "text/javascript"
          B.! A.src "/static/scripts/jquery-2.1.4.min.js"
        H.script ""
          B.! A.type_ "text/javascript"
          B.! A.src "/static/scripts/bootstrap.min.js"
        H.script ""
          B.! A.type_ "text/javascript"
          B.! A.src "/static/scripts/home.js"
      H.body $ do
        H.div B.! A.class_ "container" $
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
