{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Authentication.Utils
( getCallbackUri
, saveReferer
, redirectToCurrentRefererIfAllowed
, redirectToSavedRefererIfAllowed
, redirectToBodyRefererIfAllowed
, presentMessageAndRedirectToTargetUrl
, presentMessageAndRedirectToBodyRefererIfAllowed
, presentMessageAndRedirectToCookieRefererIfAllowed
, isAllowedReferer
, retrieveRequestHeaderRefererIfAllowed
, retrieveBodyRefererIfAllowed
, retrieveCookieRefererIfAllowed
) where

import Happstack.Server
import Data.List (isPrefixOf)
import Data.Maybe (maybe)
import Control.Applicative (optional)
import URI.ByteString (URI, parseURI, strictURIParserOptions)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BSS8
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Retrieves the appropriate callback URI based on the Host header.
getCallbackUri :: T.Text -> ServerPart URI
getCallbackUri callbackPath = do
    -- TODO: allow list for hosts
    rq <- askRq
    case getHeader "host" rq of
        Nothing -> mempty
        Just rawHost -> do
            let host = TE.decodeUtf8 rawHost
            let scheme = if (T.takeWhile (/= ':') host) `elem` ["localhost", "127.0.0.1"] then "http" else "https"
            case parseURI strictURIParserOptions $ TE.encodeUtf8 $ T.concat [scheme, "://", host, callbackPath] of
                Left _ -> mempty
                Right parsedURI -> return parsedURI

-- * Allow lists
-- | List of allowed referer prefixes.
allowedRefererPrefixes :: [T.Text]
allowedRefererPrefixes =
    [ "http://localhost:8000/"
    , "http://localhost:8080/"
    , "https://lojban.johnjq.com/"
    , "https://lojban.io/"
    ]

-- | Checks whether a given referer is allowed.
isAllowedReferer :: T.Text -> Bool
isAllowedReferer referer = any (`T.isPrefixOf` referer) allowedRefererPrefixes

-- * Plain redirects
-- | Redirects to the current referer, if allowed; otherwise, redirects to homepage.
-- TODO: rename function to "redirectToRequestHeaderRefererIfAllowed"
redirectToCurrentRefererIfAllowed :: ServerPart Response
redirectToCurrentRefererIfAllowed = do
    refererMaybe <- retrieveRequestHeaderRefererIfAllowed
    case refererMaybe of
        Nothing -> redirectToHomepage
        Just referer -> tempRedirect referer $ toResponse T.empty

-- | Redirects to the referer from the "referer" parameter in the request body, if allowed; otherwise, redirects to homepage.
redirectToBodyRefererIfAllowed :: ServerPart Response
redirectToBodyRefererIfAllowed = do
    refererMaybe <- retrieveBodyRefererIfAllowed
    case refererMaybe of
        Nothing -> redirectToHomepage
        Just referer -> tempRedirect referer $ toResponse T.empty

-- TODO: rename to "saveRefererToCookie"
-- TODO: place this function outside of the "Plain redirects" section
-- | Saves the referer from the "Referer" request header to the specified cookie.
saveReferer :: T.Text -> ServerPart ()
saveReferer refererCookieName = do
    rq <- askRq
    let refererMaybe = do
            originalReferer <- BSS8.unpack <$> getHeader "Referer" rq
            if isAllowedReferer (T.pack originalReferer) then
                return originalReferer
            else
                Nothing
    case refererMaybe of
        Just referer -> addCookie Session $ mkCookie (T.unpack refererCookieName) referer
        Nothing -> expireCookie (T.unpack refererCookieName)
    return ()

-- TODO: rename this function to "redirectToCookieRefererIfAllowed"
-- | Redirects to the referer from the specified cookie, if allowed; otherwise, redirects to homepage.
redirectToSavedRefererIfAllowed :: T.Text -> ServerPart Response
redirectToSavedRefererIfAllowed refererCookieName = do
    refererMaybe <- retrieveCookieRefererIfAllowed refererCookieName
    case refererMaybe of
        Nothing -> redirectToHomepage
        Just referer -> tempRedirect referer $ toResponse T.empty

-- | Redirects to the homepage.
redirectToHomepage :: ServerPart Response
redirectToHomepage = tempRedirect ("/" :: T.Text) $ toResponse T.empty

-- * Redirects with custom message
-- | Display a custom message and then redirects to the referer found in the "referer" parameter in the request body, if allowed; otherwise, redirects to homepage.
presentMessageAndRedirectToBodyRefererIfAllowed :: T.Text -> ServerPart Response
presentMessageAndRedirectToBodyRefererIfAllowed message = do
    let fallbackReferer = "/"
    bodyRefererMaybe <- retrieveBodyRefererIfAllowed
    case bodyRefererMaybe of
        Nothing -> presentMessageAndRedirectToTargetUrl fallbackReferer message
        Just bodyReferer -> presentMessageAndRedirectToTargetUrl bodyReferer message

-- | Display a custom message and then redirects to the referer found in the specified cookie, if allowed; otherwise, redirects to homepage.
presentMessageAndRedirectToCookieRefererIfAllowed :: T.Text -> T.Text -> ServerPart Response
presentMessageAndRedirectToCookieRefererIfAllowed refererCookieName message = do
    let fallbackReferer = "/"
    cookieRefererMaybe <- retrieveCookieRefererIfAllowed refererCookieName
    case cookieRefererMaybe of
        Nothing -> presentMessageAndRedirectToTargetUrl fallbackReferer message
        Just cookieReferer -> presentMessageAndRedirectToTargetUrl cookieReferer message

-- | Display a custom message and then redirects to the target url.
presentMessageAndRedirectToTargetUrl :: T.Text -> T.Text -> ServerPart Response
presentMessageAndRedirectToTargetUrl targetUrl message = ok $ toResponse $ do
    -- TODO: implement fallback if JS is disabled (response header for redirect + "if you are not redirected in X seconds... link" in HTML)
    let escapeString =  T.replace "\"" "\\\"" . T.replace "\\" "\\\\"
    let embeddedCode = T.concat
            [ "alert(\"" `T.append` (escapeString message) `T.append` "\");"
            , "window.location = \"" `T.append` (escapeString targetUrl) `T.append` "\";"
            ]
    H.script (H.toHtml embeddedCode)
      B.! A.type_ "text/javascript"

-- * Referer retrieval
-- | Retrieves the referer found in the "Referer" request header, if allowed; otherwise, returns `Nothing`.
retrieveRequestHeaderRefererIfAllowed :: ServerPart (Maybe T.Text)
retrieveRequestHeaderRefererIfAllowed = (fmap $ T.pack . BSS8.unpack) <$> getHeader "Referer" <$> askRq >>= retrieveSpecifiedRefererIfAllowed

-- | Retrieves the referer found in the "referer" parameter in the request body, if allowed; otherwise, returns `Nothing`.
retrieveBodyRefererIfAllowed :: ServerPart (Maybe T.Text)
retrieveBodyRefererIfAllowed = (fmap TL.toStrict) <$> (optional $ body $ lookText "referer") >>= retrieveSpecifiedRefererIfAllowed

-- | Retrieves the referer found in the specified cookie, if allowed; otherwise, returns `Nothing`.
retrieveCookieRefererIfAllowed :: T.Text -> ServerPart (Maybe T.Text)
retrieveCookieRefererIfAllowed refererCookieName = (fmap T.pack) <$> (optional $ lookCookieValue $ T.unpack refererCookieName) >>= retrieveSpecifiedRefererIfAllowed

-- | Retrieves the specified referer, if allowed; otherwise, returns `Nothing`.
retrieveSpecifiedRefererIfAllowed :: Maybe T.Text -> ServerPart (Maybe T.Text)
retrieveSpecifiedRefererIfAllowed refererMaybe = return $ do
    referer <- refererMaybe
    if isAllowedReferer referer then
        return referer
    else
        Nothing
