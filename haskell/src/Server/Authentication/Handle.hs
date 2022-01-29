{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Authentication.Handle
( handleRoot
, handleLogout
, readUserIdentityFromCookies
) where

import Server.Authentication.Utils (redirectToBodyRefererIfAllowed, presentMessageAndRedirectToTargetUrl)
import Server.Core
import Happstack.Server
import Server.Logic.Redis (runRedis, encodeRedisKey)
import Data.Char (isAscii, isAlphaNum)
import Data.Either (isLeft)
import Control.Monad (msum)
import Control.Monad.Trans (liftIO)
import qualified Database.Redis as Redis
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL

validateHandle :: T.Text -> Either String ()
validateHandle handle =
    if T.null handle then
        Left "the handle must not be empty"
    else if (not $ T.all isValidCharacterForHandle handle) then
        Left "the handle contains invalid characters"
    else if T.length handle > 60 then
        Left "the handle is too long"
    else
        Right ()
    where
        isValidCharacterForHandle :: Char -> Bool
        isValidCharacterForHandle c = isAscii c && (isAlphaNum c || isValidSymbol c)
        isValidSymbol :: Char -> Bool
        isValidSymbol c = c `elem` ['-', '_', '.']

bodyPolicy :: BodyPolicy
bodyPolicy = defaultBodyPolicy "/tmp" 0 1000 1000

handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = msum
    [ dir "login" $ handleLogin serverConfiguration serverResources
    , dir "register" $ handleRegister serverConfiguration serverResources
    ]

handleLogin :: ServerConfiguration -> ServerResources -> ServerPart Response
handleLogin serverConfiguration serverResources = do
    -- Accept only POST requests
    method POST
    -- Read handle
    decodeBody bodyPolicy
    handle <- TL.toStrict <$> (body $ lookText "existing-handle")
    -- Validate the handle
    case validateHandle handle of
        Left msg -> presentMessageAndRedirectToTargetUrl ("/login#existing" :: T.Text) $ "Invalid handle: " `T.append` (T.pack msg)
        Right () -> do
            -- Check if the handle exists
            (liftIO $ runRedis serverConfiguration serverResources $ isHandleRegistered handle) >>= \case
                Left _ -> internalServerError $ toResponse $ ("Failed to connect to the database." :: T.Text)
                Right False -> presentMessageAndRedirectToTargetUrl ("/login#existing" :: T.Text) ("This handle does not exist. If you would like to use it, please register it first." :: T.Text)
                Right True -> do
                    -- If the sign-in attempt was successful, then register the cookie and return the user to the previous page
                    addCookies $ (cookieDuration,) <$>
                        [ mkCookie (T.unpack $ handleCookieName) $ T.unpack handle
                        ]
                    redirectToBodyRefererIfAllowed

handleLogout :: ServerConfiguration -> ServerResources -> ServerPart ()
handleLogout serverConfiguration serverResources = do
    expireCookie (T.unpack $ handleCookieName)

handleRegister :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRegister serverConfiguration serverResources = do
    -- Accept only POST requests
    method POST
    -- Read handle
    decodeBody bodyPolicy
    handle <- TL.toStrict <$> (body $ lookText "new-handle")
    -- Validate the handle
    case validateHandle handle of
        Left msg -> presentMessageAndRedirectToTargetUrl ("/login#register" :: T.Text) $ "Invalid handle: " `T.append` (T.pack msg)
        Right () -> do
            -- Attempt to register the handle
            (liftIO $ runRedis serverConfiguration serverResources $ registerHandle handle) >>= \case
                Left _ -> internalServerError $ toResponse $ ("Failed to connect to the database." :: T.Text)
                Right False -> presentMessageAndRedirectToTargetUrl ("/login#register" :: T.Text) ("This handle is not available. Please try a different one." :: T.Text)
                Right True -> do
                    -- If the handle was successfully registered, then register the cookie and return the user to the previous page
                    addCookies $ (cookieDuration,) <$>
                        [ mkCookie (T.unpack $ handleCookieName) $ T.unpack handle
                        ]
                    redirectToBodyRefererIfAllowed

readUserIdentityFromCookies :: ServerConfiguration -> ServerResources -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookies serverConfiguration serverResources = do
    handle <- T.pack <$> (lookCookieValue (T.unpack handleCookieName))
    if T.null handle then
        return Nothing
    else if (isLeft $ validateHandle handle) then
        return Nothing
    else
        (liftIO $ runRedis serverConfiguration serverResources $ isHandleRegistered handle) >>= \case
            Left _ -> do
                -- Failed to connect to the database
                handleLogout serverConfiguration serverResources
                return Nothing
            Right False -> do
                -- The handle did not exist in the database
                handleLogout serverConfiguration serverResources
                return Nothing
            Right True -> do
                let userIdentifier = UserIdentifier "handle" handle
                let userPictureUrl = T.empty
                let userName = T.empty
                return . Just $ UserIdentity userIdentifier userPictureUrl userName

-- * Redis bindings
registeredHandleKey :: T.Text -> T.Text
registeredHandleKey handle = "RegisteredHandle" `T.append` handleKey where
    normalizedHandle = T.toLower handle
    handleKey = encodeRedisKey
        [ ("handle", normalizedHandle)
        ]

isHandleRegistered :: T.Text -> Redis.Redis (Either Redis.Reply Bool)
isHandleRegistered handle = do
    let key = registeredHandleKey handle
    let encodedKey = TE.encodeUtf8 key
    Redis.exists encodedKey

registerHandle :: T.Text -> Redis.Redis (Either Redis.Reply Bool)
registerHandle handle = do
    let key = registeredHandleKey handle
    let encodedKey = TE.encodeUtf8 key
    Redis.exists encodedKey >>= \case
        Left reply ->
            -- Something went wrong while communicating with Redis
            return $ Left reply
        Right True ->
            -- If the handle already existed, then the request to register it was not successful
            return $ Right False
        Right False -> do
            -- If the handle is available, then we register it and return success
            _ <- Redis.set encodedKey (TE.encodeUtf8 T.empty)
            return $ Right True

-- * Cookies
handleCookieName :: T.Text
handleCookieName = "handle_id"

cookieDuration :: CookieLife
cookieDuration = (MaxAge $ 30 * 86400)
