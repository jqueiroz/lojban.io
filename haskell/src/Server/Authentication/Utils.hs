{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Authentication.Utils
( getCallbackUri
, redirectToCurrentRefererIfAllowed
, saveReferer
, redirectToSavedRefererIfAllowed
, isAllowedReferer
) where

import Happstack.Server
import Data.List (isPrefixOf)
import URI.ByteString (URI, parseURI, strictURIParserOptions)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BSS8

getCallbackUri :: T.Text -> ServerPart URI
getCallbackUri callbackPath = do
    rq <- askRq
    case getHeader "host" rq of
        Nothing -> mempty
        Just rawHost -> do
            let host = TE.decodeUtf8 rawHost
            let scheme = if (T.takeWhile (/= ':') host) `elem` ["localhost", "127.0.0.1"] then "http" else "https"
            case parseURI strictURIParserOptions $ TE.encodeUtf8 $ T.concat [scheme, "://", host, callbackPath] of
                Left _ -> mempty
                Right parsedURI -> return parsedURI

allowedRefererPrefixes :: [String]
allowedRefererPrefixes =
    [ "http://localhost:8000/"
    , "http://localhost:8080/"
    , "https://lojban.johnjq.com/"
    , "https://lojban.io/"
    ]

isAllowedReferer :: String -> Bool
isAllowedReferer referer = any (`isPrefixOf` referer) allowedRefererPrefixes

redirectToCurrentRefererIfAllowed :: ServerPart Response
redirectToCurrentRefererIfAllowed = do
    refererMaybe <- getHeader "Referer" <$> askRq
    case refererMaybe of
        Nothing -> do
            -- Redirect the homepage
            tempRedirect ("/" :: T.Text) $ toResponse ("" :: T.Text)
        Just referer -> do
            let referer' = BSS8.unpack referer
            if isAllowedReferer referer' then
                -- Redirect to referer
                tempRedirect referer' $ toResponse ("" :: T.Text)
            else
                -- Redirect the homepage
                tempRedirect ("/" :: T.Text) $ toResponse ("" :: T.Text)

saveReferer :: T.Text -> ServerPart ()
saveReferer refererCookieName = do
    rq <- askRq
    let refererMaybe = do
            originalReferer <- BSS8.unpack <$> getHeader "Referer" rq
            if isAllowedReferer originalReferer then
                return originalReferer
            else
                Nothing
    case refererMaybe of
        Just referer -> addCookie Session $ mkCookie (T.unpack refererCookieName) referer
        Nothing -> expireCookie (T.unpack refererCookieName)
    return ()

redirectToSavedRefererIfAllowed :: T.Text -> ServerPart Response
redirectToSavedRefererIfAllowed refererCookieName = do
    referer <- lookCookieValue (T.unpack refererCookieName)
    if isAllowedReferer referer then
        -- Redirect to referer
        tempRedirect referer $ toResponse ("" :: T.Text)
    else
        -- Redirect the homepage
        tempRedirect ("/" :: T.Text) $ toResponse ("" :: T.Text)
