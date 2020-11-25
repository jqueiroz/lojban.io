{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Login
( displayLoginHome
) where

import Server.Core
import Server.Website.Views.Core
import Data.Maybe (isJust)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

displayLoginHome :: ServerConfiguration -> Maybe UserIdentity -> Maybe T.Text -> UUID.UUID -> H.Html
displayLoginHome serverConfiguration userIdentityMaybe refererMaybe uuid = do
    when (isJust userIdentityMaybe) $ error "You are already signed in."
    let shortDescription = ("Register a new handle or sign in with an existing one. After doing so, you will be able to use decks and have your progress on individual cards tracked." :: T.Text)
    H.docType
    H.html B.! A.lang (H.textValue "en-us") $ do
        H.head $ do
            H.title $ H.toHtml ("Register or sign in :: lojban.io" :: T.Text)
            H.meta B.! A.name (H.textValue "description") B.! A.content (H.textValue shortDescription)
            includeUniversalStylesheets
            includeUniversalScripts
            includeInternalStylesheet "login.css"
            includeInternalScript "login.js"
        H.body $ do
            displayTopbar serverConfiguration userIdentityMaybe TopbarNone
            H.div B.! A.class_ (H.textValue "main") $ do
                H.div B.! A.class_ (H.textValue "header") $ do
                    H.div B.! A.class_ (H.textValue "header-bg") $ H.toHtml ("" :: T.Text)
                    H.h1 $ H.toHtml ("Register or sign in" :: T.Text)
                    H.p $ H.toHtml shortDescription
                H.div B.! A.class_ (H.textValue "body") $ do
                    displaySignIn refererMaybe uuid
                    displayFooter

displaySignIn :: Maybe T.Text -> UUID.UUID -> H.Html
displaySignIn refererMaybe uuid = do
    -- TODO: captcha
    -- TODO: check "#login" and "#register" to determine the initially selected value of the radio button
    let handlePattern = "[a-zA-Z0-9-_.]+" :: T.Text
    let maxHandleLength = "60" :: T.Text
    let refererTag = case refererMaybe of
            Nothing -> mempty
            Just referer ->
                -- Check if the referer is the login page itself, or the authentication page, to prevent infinite redirects
                if ("login" `T.isInfixOf` referer) || ("authentication" `T.isInfixOf` referer)
                    then mempty
                    else H.input B.! A.name (H.textValue "referer") B.! A.type_ (H.textValue "hidden") B.! A.value (H.textValue referer)
    H.div B.! A.class_ (H.textValue "login-main") $ do
        H.input B.! A.type_ (H.textValue "radio") B.! A.name (H.textValue "login-input") B.! A.id (H.textValue "login-input-existing") B.! A.class_ (H.textValue "login-input-existing") B.! A.checked (H.textValue "checked")
        H.input B.! A.type_ (H.textValue "radio") B.! A.name (H.textValue "login-input") B.! A.id (H.textValue "login-input-register") B.! A.class_ (H.textValue "login-input-register")
        H.label B.! A.for (H.textValue "login-input-existing") $ do
            H.h2 $ do
                H.span B.! A.class_ (H.textValue "fas fa-user") $ H.toHtml T.empty
                H.toHtml ("Sign in with an existing handle" :: T.Text)
        H.label B.! A.for (H.textValue "login-input-register") $ do
            H.h2 $ do
                H.span B.! A.class_ (H.textValue "fas fa-plus-square") $ H.toHtml T.empty
                H.toHtml ("Register a new handle" :: T.Text)
        H.div B.! A.class_ (H.textValue "login-existing") $ do
            H.h3 $ H.toHtml ("If you have already registered a handle, you may sign in using it." :: T.Text)
            H.form B.! A.action (H.textValue "/authentication/handle/login") B.! A.method (H.textValue "POST") $ do
                refererTag
                H.label B.! A.for (H.textValue "existing-handle") $ H.toHtml ("Handle" :: T.Text)
                H.input B.! A.class_ (H.textValue "handle") B.! A.name (H.textValue "existing-handle") B.! A.id (H.textValue "existing-handle") B.! A.pattern (H.textValue handlePattern) B.! A.maxlength (H.textValue maxHandleLength)
                H.div B.! A.class_ (H.textValue "buttons") $ do
                    H.button $ H.toHtml $ ("Sign in" :: T.Text)
        H.div B.! A.class_ (H.textValue "login-register") $ do
            H.h3 $ H.toHtml ("You may register a new handle, if you do not have one already." :: T.Text)
            H.input B.! A.type_ (H.textValue "radio") B.! A.name (H.textValue "login-register-input") B.! A.id (H.textValue "login-register-input-random") B.! A.class_ (H.textValue "login-register-input-random") B.! A.checked (H.textValue "checked")
            H.label B.! A.for (H.textValue "login-register-input-random") $ H.toHtml ("Use a randomly generated handle" :: T.Text)
            H.input B.! A.type_ (H.textValue "radio") B.! A.name (H.textValue "login-register-input") B.! A.id (H.textValue "login-register-input-custom") B.! A.class_ (H.textValue "login-register-input-custom")
            -- TODO: button for easy copy-pasting
            H.label B.! A.for (H.textValue "login-register-input-custom") $ H.toHtml ("Choose a custom handle" :: T.Text)
            H.div B.! A.class_ ("login-register-random") $ do
                H.form B.! A.action (H.textValue "/authentication/handle/register") B.! A.method (H.textValue "POST") $ do
                    refererTag
                    H.label B.! A.for (H.textValue "new-handle-random") $ H.toHtml ("Generated handle" :: T.Text)
                    H.input B.! A.class_ (H.textValue "handle") B.! A.name (H.textValue "new-handle") B.! A.id (H.textValue "new-handle-random") B.! A.value (H.textValue $ UUID.toText uuid) B.! A.readonly mempty B.! A.pattern (H.textValue handlePattern) B.! A.maxlength (H.textValue maxHandleLength)
                    -- TODO: side button for easily copying the handle
                    H.p $ do
                        H.b $ H.toHtml ("Remark: " :: T.Text)
                        H.span $ H.toHtml ("Please safely store a copy of this randomly generated identifier." :: T.Text)
                        H.span $ H.toHtml (" In the event that you lose it, you will be unable to restore your progress." :: T.Text)
                    H.div B.! A.class_ (H.textValue "buttons") $ do
                        H.button $ H.toHtml $ ("Register" :: T.Text)
            H.div B.! A.class_ ("login-register-custom") $ do
                H.form B.! A.action (H.textValue "/authentication/handle/register") B.! A.method (H.textValue "POST") $ do
                    refererTag
                    H.label B.! A.for (H.textValue "new-handle-custom") $ H.toHtml ("Custom handle" :: T.Text)
                    H.input B.! A.class_ (H.textValue "handle") B.! A.name (H.textValue "new-handle") B.! A.id (H.textValue "new-handle-custom") B.! A.pattern (H.textValue handlePattern) B.! A.maxlength (H.textValue maxHandleLength)
                    H.p $ do
                        H.b $ H.toHtml ("Warning: " :: T.Text)
                        H.span $ H.toHtml ("For your convenience, we accept the use of a custom identifier in lieu of a randomly generated one." :: T.Text)
                        H.span $ H.toHtml (" If you choose this mechanism, please beware that anyone who shares your handle, or anyone who knows your handle, will be able to interfere with your progress." :: T.Text)
                    H.p $ do
                        H.span $ H.toHtml ("For information on why we do not accept passwords, please refer to " :: T.Text)
                        H.a "Issue #8: Allow signing in or registering without a google account"
                          B.! A.href "https://github.com/jqueiroz/lojban.io/issues/8#issuecomment-705981627"
                        H.span $ H.toHtml ("." :: T.Text)
                    H.div B.! A.class_ (H.textValue "buttons") $ do
                        H.button $ H.toHtml $ ("Register" :: T.Text)
