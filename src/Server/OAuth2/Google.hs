{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.OAuth2.Google
( handleRoot
, readUserIdentityFromCookies
) where

import GHC.Generics
import Server.Core
import Server.OAuth2.Core
import Happstack.Server
import System.Environment (getEnv)
import Data.Either.Combinators (rightToMaybe)
import Control.Monad (msum)
import Control.Monad.Extra (liftMaybe)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import URI.ByteString.QQ (uri)
import URI.ByteString (serializeURIRef')
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Jose.Jwk as JWK
import qualified Jose.Jwt as JWT
import qualified Web.OIDC.Client as OIDC
import qualified Network.OAuth.OAuth2 as OA2
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Claims = Claims
    { email :: T.Text
    , email_verified :: Bool
    } deriving (Generic, Show)

instance A.FromJSON Claims where
    parseJSON = A.genericParseJSON A.defaultOptions

data UserInfo = UserInfo
    { given_name :: T.Text
    , family_name :: T.Text
    , picture :: T.Text
    } deriving (Generic, Show)

instance A.FromJSON UserInfo where
    parseJSON = A.genericParseJSON A.defaultOptions

identityTokenCookieName :: String
identityTokenCookieName = "google_identityToken"

userInfoCookieName :: String
userInfoCookieName = "google_userInfo"

readUserIdentityFromCookies :: ServerResources -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookies serverResources = do
    -- Fetch cookie values
    identityTokenText <- T.pack <$> lookCookieValue identityTokenCookieName
    userInfoByteString <- BS.pack <$> lookCookieValue userInfoCookieName
    -- Extract claims
    claimsMaybe <- liftIO $ runMaybeT $ extractClaims serverResources identityTokenText
    case claimsMaybe of
        Nothing -> return Nothing
        Just claims -> do
            -- Decode user info
            let userInfoMaybe = A.decode userInfoByteString :: Maybe UserInfo
            case userInfoMaybe of
                Nothing -> return Nothing
                Just userInfo -> do
                    let userIdentifier = UserIdentifier "google" (email claims)
                    let userPictureUrl = picture userInfo
                    let userGivenName = given_name userInfo
                    let userFamilyName = family_name userInfo
                    return . Just $ UserIdentity userIdentifier userPictureUrl userGivenName userFamilyName

extractClaims :: ServerResources -> T.Text -> MaybeT IO Claims
extractClaims serverResources identityTokenText = do
    -- Decode jwt token
    googlePublicKeys <- liftIO $ getGooglePublicKeys serverResources
    jwtTokenEither <- liftIO $ JWT.decode googlePublicKeys Nothing (TE.encodeUtf8 identityTokenText)
    jwtToken <- liftMaybe $ rightToMaybe jwtTokenEither
    jwsPayload <- liftMaybe $ do
        case jwtToken of
            JWT.Jws (jwsHeader, jwsPayload) -> Just jwsPayload
            _ -> Nothing
    -- Extract claims
    claims <- liftMaybe $ A.decodeStrict jwsPayload
    -- Validate claims
    if (email_verified claims)
        then return claims
        else liftMaybe Nothing

handleRoot :: ServerResources -> ServerPart Response
handleRoot serverResources = msum
    [ dir "login" $ handleLogin
    , dir "callback" $ handleCallback serverResources
    ]

handleLogin :: ServerPart Response
handleLogin = do
    authorizationUrl <- liftIO getAuthorizationUrl
    tempRedirect authorizationUrl $ toResponse ("" :: T.Text)

handleCallback :: ServerResources -> ServerPart Response
handleCallback serverResources = do
    -- Retrieve exchange token from querystring
    -- TODO: also handle the 'state' parameter
    code <- lookText' "code"
    let exchangeToken = OA2.ExchangeToken code
    -- Acquire oauth2 token from Google
    let tlsManager = serverResourcesTlsManager serverResources
    oauth2Config <- liftIO getOAuth2Config
    oauth2TokenEither <- liftIO $ OA2.fetchAccessToken tlsManager oauth2Config exchangeToken
    case oauth2TokenEither of
        Left _ -> unauthorized $ toResponse ("Acquisition of oauth2 token failed." :: T.Text)
        Right oauth2Token -> do
            -- Extract access token
            let accessToken = OA2.accessToken oauth2Token
            liftIO $ print accessToken
            -- Extract identity token
            case  OA2.idtoken <$> (OA2.idToken oauth2Token) of
                Nothing -> unauthorized $ toResponse ("Acquisition of identity token failed." :: T.Text)
                Just identityTokenText -> do
                    -- Extract claims
                    claimsMaybe <- liftIO $ runMaybeT $ extractClaims serverResources identityTokenText
                    case claimsMaybe of
                        Nothing -> unauthorized $ toResponse ("Decoding of identity token failed." :: T.Text)
                        Just claims -> do
                            -- Fetch user info
                            userInfoByteString <- liftIO $ fetchUserInfo serverResources accessToken
                            -- Validate user info
                            let userInfoMaybe = A.decode userInfoByteString :: Maybe UserInfo
                            case userInfoMaybe of
                                Nothing -> unauthorized $ toResponse ("Decoding of user info failed." :: T.Text)
                                Just userInfo -> do
                                    -- Save identity token and user info to cookies
                                    let cookieDuration = (MaxAge $ 30 * 86400)
                                    addCookies $ (cookieDuration,) <$>
                                        [ mkCookie identityTokenCookieName (T.unpack identityTokenText)
                                        , mkCookie userInfoCookieName (BS.unpack userInfoByteString)
                                        ]
                                    -- Redirect user to the homepage
                                    tempRedirect ("/" :: T.Text) $ toResponse ("" :: T.Text)

fetchUserInfo :: ServerResources -> OA2.AccessToken -> IO BS.ByteString
fetchUserInfo serverResources accessToken = do
    let tlsManager = serverResourcesTlsManager serverResources
    let accessTokenString = T.unpack $ OA2.atoken accessToken
    request <- HC.parseRequest $ "https://www.googleapis.com/oauth2/v3/userinfo?access_token=" ++ accessTokenString
    response <- HC.httpLbs request tlsManager
    return $ HC.responseBody response

getGooglePublicKeys :: ServerResources -> IO [JWK.Jwk]
getGooglePublicKeys serverResources = do
    let tlsManager = serverResourcesTlsManager serverResources
    provider <- OIDC.discover "https://accounts.google.com" tlsManager
    return $ OIDC.jwkSet provider

getOAuth2Config :: IO OA2.OAuth2
getOAuth2Config = do
    clientId <- getEnv "LOJBAN_TOOL_OAUTH2_GOOGLE_CLIENT_ID"
    clientSecret <- getEnv "LOJBAN_TOOL_OAUTH2_GOOGLE_CLIENT_SECRET"
    return $ OA2.OAuth2
        { OA2.oauthClientId = T.pack clientId
        , OA2.oauthClientSecret = T.pack clientSecret
        , OA2.oauthCallback = Just [uri|http://127.0.0.1:8000/oauth2/google/callback|]
        , OA2.oauthOAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/auth|]
        , OA2.oauthAccessTokenEndpoint = [uri|https://www.googleapis.com/oauth2/v3/token|]
        }

getAuthorizationUrl :: IO T.Text
getAuthorizationUrl = do
    oauth2Config <- getOAuth2Config
    let params = [ ("scope", "email profile") ]
    let url = OA2.appendQueryParams params $ OA2.authorizationUrl oauth2Config
    return $ TE.decodeUtf8 $ serializeURIRef' url
