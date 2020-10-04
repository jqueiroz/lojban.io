{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Authentication.Utils
( isAllowedReferer
, redirectToCurrentRefererIfAllowed
) where

import Happstack.Server
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSS8

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
