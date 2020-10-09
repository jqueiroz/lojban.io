{-# LANGUAGE OverloadedStrings #-}

module Server.Website.Views.Core
( includeUniversalStylesheets
, includeInternalStylesheet
, includeExternalStylesheet
, includeInlineStylesheet
, includeCourseStylesheet
, includeUniversalScripts
, includeInternalScript
, includeExternalScript
, includeInlineScript
, includeDictionaryScript
, includeDeckScript
, includeCourseScript
, includeLessonScript
, TopbarCategory (..)
, displayTopbar
, displayFooter
) where

import Core
import Server.Core
import Language.Lojban.Core
import Control.Monad (unless, forM_)
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- * Stylesheets
includeViewportTag :: H.Html
includeViewportTag = do
    H.meta
      B.! A.name (H.stringValue "viewport")
      --TODO: B.! A.content "width=device-width, initial-scale=1"
      B.! A.content ""

includeThemeColorTag :: H.Html
includeThemeColorTag = do
    H.meta
      B.! A.name (H.stringValue "theme-color")
      B.! A.content "#9054FF"

includeOpenGraphImageTag :: H.Html
includeOpenGraphImageTag = do
    let imageUrl = "https://live.staticflickr.com/65535/50395936413_e56cf07308_o.png"
    H.meta
      B.! H.customAttribute "property" (H.textValue "og:image")
      B.! A.content (H.textValue imageUrl)
    H.meta
      B.! H.customAttribute "property" (H.textValue "og:image:secure_url")
      B.! A.content (H.textValue imageUrl)
    H.meta
      B.! H.customAttribute "property" (H.textValue "og:image:type")
      B.! A.content "image/png"
    H.meta
      B.! H.customAttribute "property" (H.textValue "og:image:width")
      B.! A.content "1200"
    H.meta
      B.! H.customAttribute "property" (H.textValue "og:image:height")
      B.! A.content "627"

includeWebManifest :: H.Html
includeWebManifest = do
    H.link
      B.! A.href (H.stringValue "/manifest.webmanifest")
      B.! A.rel "manifest"

includeUniversalStylesheets :: H.Html
includeUniversalStylesheets = do
    includeViewportTag
    includeThemeColorTag
    includeOpenGraphImageTag
    includeWebManifest
    -- TODO: consider removing bootstrap
    includeInternalStylesheet "bootstrap.min.css"
    --includeInternalStylesheet "normalize.css"
    --includeExternalStylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
    --includeExternalStylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"

includeInternalStylesheet :: String -> H.Html
includeInternalStylesheet = includeExternalStylesheet . ("/static/style/"++)

includeExternalStylesheet :: String -> H.Html
includeExternalStylesheet src =
    H.link
      B.! A.href (H.stringValue src)
      B.! A.rel "stylesheet"

includeInlineStylesheet :: String -> H.Html
includeInlineStylesheet code =
    H.style B.! A.type_ (H.stringValue "text/css") $
        H.toHtml code

includeCourseStylesheet :: Course -> H.Html
includeCourseStylesheet course = includeInlineStylesheet code where
    style = courseStyle course
    courseColor1 = case (courseStyleColor1 style) of
        Just color ->"--course-color1: " ++ color ++ ";"
        Nothing -> ""
    courseIcon = case (courseStyleIconUrl style) of
        Just url -> "--course-icon: url(" ++ url ++ ");"
        Nothing -> ""
    code = concat
        [ ":root {"
        , courseColor1
        , courseIcon
        , "}"
        ]

-- * Scripts
includeGoogleAnalyticsScript :: H.Html
includeGoogleAnalyticsScript = do
    let embeddedCode = T.concat
            [ "window.dataLayer = window.dataLayer || [];"
            , "function gtag(){dataLayer.push(arguments);};"
            , "gtag('js', new Date());"
            , "gtag('config', 'UA-175660110-1', { 'anonymize_ip': true });"
            ]
    H.script ""
      B.! A.type_ "text/javascript"
      B.! A.src (H.stringValue "https://www.googletagmanager.com/gtag/js?id=UA-175660110-1")
      B.! A.async (H.stringValue "true")
    H.script (H.toHtml embeddedCode)
      B.! A.type_ "text/javascript"

includePwaBuilderScript :: H.Html
includePwaBuilderScript = do
    let embeddedCode = T.concat
            [ "import 'https://cdn.jsdelivr.net/npm/@pwabuilder/pwaupdate';"
            , "const el = document.createElement('pwa-update');"
            , "document.body.appendChild(el);"
            ]
    H.script (H.toHtml embeddedCode)
      B.! A.type_ "module"

includeUniversalScripts :: H.Html
includeUniversalScripts = do
    includePwaBuilderScript
    includeExternalScript "https://kit.fontawesome.com/ae6f2dc037.js"
    includeGoogleAnalyticsScript
    --includeInternalScript "vendors.js"

includeInternalScript :: String -> H.Html
includeInternalScript = includeExternalScript . ("/static/scripts/"++)

includeExternalScript :: String -> H.Html
includeExternalScript src =
    H.script ""
      B.! A.type_ "text/javascript"
      B.! A.src (H.stringValue src)
      --TODO: B.! A.crossorigin (H.stringValue "anonymous")

includeInlineScript :: T.Text -> H.Html
includeInlineScript code =
    H.script (H.toHtml code) B.! A.type_ "text/javascript"

includeDictionaryScript :: Dictionary -> H.Html
includeDictionaryScript dictionary = includeInternalScript $ T.unpack $ "dictionaries/" `T.append` (dictIdentifier dictionary) `T.append` ".js"

includeDeckScript :: Deck -> H.Html
includeDeckScript deck = includeInlineScript $ "deckId = \"" `T.append` (deckId deck) `T.append` "\";"

includeCourseScript :: Course -> H.Html
includeCourseScript course = includeInlineScript $ "courseId = \"" `T.append` (courseId course) `T.append` "\";"

includeLessonScript :: Int -> H.Html
includeLessonScript lessonNumber = includeInlineScript $ "lessonNumber = \"" `T.append` (T.pack $ show lessonNumber) `T.append` "\";"

-- * Topbar
data TopbarCategory = TopbarHome | TopbarCourses | TopbarDecks | TopbarResources | TopbarNone deriving (Enum, Eq)

displayTopbar :: ServerConfiguration -> Maybe UserIdentity -> TopbarCategory -> H.Html
displayTopbar serverConfiguration userIdentityMaybe topbarCategory = do
    H.div B.! A.class_ (H.stringValue "topbar") $ do
        H.div B.! A.class_ "logo" $ do
            H.a (H.toHtml ("lojban.io" :: String))
                B.! A.href (H.stringValue "/")
        displayTopbarMenu topbarCategory
        displayUserProfile serverConfiguration userIdentityMaybe

displayUserProfile :: ServerConfiguration -> Maybe UserIdentity -> H.Html
displayUserProfile serverConfiguration userIdentityMaybe = do
    case userIdentityMaybe of
        Nothing ->
            case (serverConfigurationIdentityProviders serverConfiguration) of
                [] -> mempty
                (identityProvidersHead : identityProvidersTail) -> do
                    H.div B.! A.class_ "user-signin" $ do
                        displayIdentityProviderSignInLink identityProvidersHead
                        H.input
                            B.! A.id "signin-menu-input"
                            B.! A.type_ "checkbox"
                        H.label
                            B.! A.for "signin-menu-input"
                            B.! A.tabindex "0"
                            B.! A.alt "Toggle sign-in menu"
                            B.! A.style (if null identityProvidersTail then "visibility: hidden;" else "") $ mempty
                        unless (null identityProvidersTail) $ do
                            H.ul B.! A.class_ "signin-menu" $ do
                                forM_ identityProvidersTail $ \identityProvider -> do
                                    H.li $ do
                                        displayIdentityProviderSignInLink identityProvider
        Just userIdentity -> do
            let pictureUrl = userPictureUrl userIdentity
            H.div B.! A.class_ "user-profile" $ do
                H.input
                    B.! A.id "user-menu-input"
                    B.! A.type_ "checkbox"
                H.label
                    B.! A.for "user-menu-input"
                    B.! A.tabindex "0"
                    B.! A.alt "Toggle user menu" $ do
                        let finalPictureUrl = if (T.null pictureUrl) then "https://www.gravatar.com/avatar/00000000000000000000000000000000?d=mp&f=y" else pictureUrl
                        H.img B.! A.class_ "picture" B.! A.src (H.textValue finalPictureUrl)
                H.ul B.! A.class_ "user-menu" $ do
                    H.li $ do
                        H.a (H.toHtml ("Sign out" :: T.Text))
                            B.! A.href (H.textValue $ "/authentication/logout/")

displayIdentityProviderSignInLink :: IdentityProvider -> H.Html
displayIdentityProviderSignInLink identityProvider = do
    H.a (H.toHtml ("sign in" :: T.Text)) B.! A.class_ (H.textValue $ "provider-" `T.append` (identityProviderIdentifier identityProvider))
        B.! A.href (H.textValue $ identityProviderLoginUrl identityProvider)
        B.! A.title (H.textValue $ "Sign in with " `T.append` (identityProviderName identityProvider))

displayTopbarMenu :: TopbarCategory -> H.Html
displayTopbarMenu topbarCategory = do
    H.ul $ do
        displayTopbarMenuItem (topbarCategory == TopbarCourses) "Courses" "/courses/"
        displayTopbarMenuItem (topbarCategory == TopbarDecks) "Decks" "/decks/"
        displayTopbarMenuItem (topbarCategory == TopbarResources) "Resources" "/resources/"

displayTopbarMenuItem :: Bool -> String -> String -> H.Html
displayTopbarMenuItem selected text url = do
    let selectedClass = if selected then "selected" else ""
    H.li $ do
        H.a (H.toHtml text)
            B.! A.href (H.stringValue url)
            B.! A.class_ selectedClass

displayFooter :: H.Html
displayFooter = do
    H.div B.! A.class_ (H.textValue "footer") $ do
        H.div B.! A.class_ (H.textValue "links") $ do
            --H.a B.! A.href (H.textValue "/about") $ H.toHtml ("About" :: T.Text)
            H.a B.! A.href (H.textValue "https://github.com/jqueiroz/lojban.io") $ H.toHtml ("About" :: T.Text)
            H.a B.! A.href (H.textValue "mailto:contact@lojban.io") $ H.toHtml ("Contact" :: T.Text)
            H.a B.! A.href (H.textValue "https://github.com/jqueiroz/lojban.io") $ H.toHtml ("Contribute" :: T.Text)
