{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.Authentication.OpenID
( handleRoot
, handleLogout
, readUserIdentityFromCookies
) where

import Server.Core
import Server.Authentication.Utils (getCallbackUri, redirectToCurrentRefererIfAllowed, saveReferer, redirectToSavedRefererIfAllowed)
import GHC.Generics
import Happstack.Server
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad (msum, mzero, forM_)
import Control.Monad.Extra (liftMaybe)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, except, withExceptT)
import URI.ByteString (URI, parseURI, strictURIParserOptions, serializeURIRef')
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Jose.Jwt as JWT
import qualified Web.OIDC.Client as OIDC
import qualified Web.OIDC.Client.Settings as OIDCS
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import qualified Network.OAuth.OAuth2 as OA2

-- * Data types

data KnownOpenIdProvider = KnownOpenIdProvider
    { knownProviderIdentifier :: T.Text
    , knownProviderDiscoveryUrl :: T.Text
    , knownProviderClientId :: T.Text
    , knownProviderClientSecret :: T.Text
    , knownProviderExtraScopes :: [T.Text]
    }

data Claims = Claims
    { sub :: T.Text
    } deriving (Generic, Show)

instance A.FromJSON Claims where
    parseJSON = A.genericParseJSON A.defaultOptions

data UserInfo = UserInfo
    { given_name :: T.Text
    , family_name :: T.Text
    , picture :: T.Text
    , email :: T.Text
    } deriving (Generic, Show)

instance A.FromJSON UserInfo where
    parseJSON = A.genericParseJSON A.defaultOptions

instance A.ToJSON UserInfo where
    toEncoding = A.genericToEncoding A.defaultOptions

-- * Known providers

knownProviders :: ServerConfiguration -> [KnownOpenIdProvider]
knownProviders serverConfiguration = catMaybes [maybeMicrosoft] where
    maybeMicrosoft :: Maybe KnownOpenIdProvider
    maybeMicrosoft = (KnownOpenIdProvider "microsoft" discoveryUrl) <$> maybeClientId <*> maybeClientSecret <*> (Just extraScopes) where
        discoveryUrl = "https://login.microsoftonline.com/consumers/v2.0"
        maybeClientId :: Maybe T.Text
        maybeClientId = (T.pack <$> serverConfigurationOpenIdMicrosoftClientId serverConfiguration)
        maybeClientSecret :: Maybe T.Text
        maybeClientSecret = (T.pack <$> serverConfigurationOpenIdMicrosoftClientSecret serverConfiguration)
        extraScopes = ["User.Read"]

-- * Handlers

handleRoot :: ServerConfiguration -> ServerResources -> ServerPart Response
handleRoot serverConfiguration serverResources = msum $ map applyProvider (knownProviders serverConfiguration) where
    applyProvider :: KnownOpenIdProvider -> ServerPart Response
    applyProvider provider = dir (T.unpack $ knownProviderIdentifier provider) $ handleRootForProvider serverConfiguration serverResources provider

handleRootForProvider :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> ServerPart Response
handleRootForProvider serverConfiguration serverResources provider = msum
    [ dir "login" $ handleLogin serverConfiguration serverResources provider
    , dir "callback" $ handleCallback serverConfiguration serverResources provider
    ]

handleLogin :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> ServerPart Response
handleLogin serverConfiguration serverResources knownProvider = do
    saveReferer (providerRefererCookieName knownProvider)
    discoveredProvider <- discoverProvider serverConfiguration serverResources knownProvider
    let scopes = [OIDC.email, OIDC.profile] ++ (knownProviderExtraScopes knownProvider)
    authenticationUrl <- liftIO $ OIDC.getAuthenticationRequestUrl discoveredProvider scopes Nothing []
    tempRedirect authenticationUrl $ toResponse ("" :: T.Text)

handleLogout :: ServerConfiguration -> ServerResources -> ServerPart ()
handleLogout serverConfiguration serverResources = forM_ (knownProviders serverConfiguration) (handleLogoutForProvider serverConfiguration serverResources)

handleLogoutForProvider :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> ServerPart ()
handleLogoutForProvider serverConfiguration serverResources knownProvider = do
    expireCookie (T.unpack $ providerIdentityTokenCookieName knownProvider)
    expireCookie (T.unpack $ providerUserInfoCookieName knownProvider)
    expireCookie (T.unpack $ providerRefererCookieName knownProvider)

-- TODO: simplify these pattern matches using ExceptionT
handleCallback :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> ServerPart Response
handleCallback serverConfiguration serverResources knownProvider = do
    -- Retrieve exchange token from querystring
    code <- lookText' "code"
    let exchangeToken = OA2.ExchangeToken code
    -- Acquire oauth2 token
    discoveredProvider <- discoverProvider serverConfiguration serverResources knownProvider
    oauth2ConfigEither <- getProviderOAuth2Config serverConfiguration serverResources knownProvider discoveredProvider
    case oauth2ConfigEither of
        Left _ -> unauthorized $ toResponse ("Acquisition of oauth2 config failed." :: T.Text)
        Right oauth2Config -> do
            let tlsManager = serverResourcesTlsManager serverResources
            oauth2TokenEither <- liftIO $ OA2.fetchAccessToken tlsManager oauth2Config exchangeToken
            case oauth2TokenEither of
                Left _ -> unauthorized $ toResponse ("Acquisition of oauth2 token failed." :: T.Text)
                Right oauth2Token -> do
                    -- Extract access token
                    let accessToken = OA2.accessToken oauth2Token
                    -- Extract identity token
                    case  OA2.idtoken <$> (OA2.idToken oauth2Token) of
                        Nothing -> unauthorized $ toResponse ("Acquisition of identity token failed." :: T.Text)
                        Just identityTokenText -> do
                            -- Extract claims
                            claimsMaybe <- liftIO $ runMaybeT $ extractClaims serverConfiguration serverResources knownProvider discoveredProvider identityTokenText
                            case claimsMaybe of
                                Nothing -> unauthorized $ toResponse ("Decoding of identity token failed." :: T.Text)
                                Just claims -> do
                                    -- Fetch user info
                                    originalUserInfoTextMaybe :: Maybe T.Text <- liftIO $ fetchUserInfo serverConfiguration serverResources knownProvider discoveredProvider accessToken
                                    case originalUserInfoTextMaybe of
                                        Nothing -> unauthorized $ toResponse ("Fetching of user info failed." :: T.Text)
                                        Just originalUserInfoText -> do
                                            -- Validate user info
                                            let originalUserInfoMaybe = (A.decodeStrict . TE.encodeUtf8) originalUserInfoText :: Maybe UserInfo
                                            case originalUserInfoMaybe of
                                                Nothing -> unauthorized $ toResponse ("Decoding of user info failed." :: T.Text)
                                                Just originalUserInfo -> do
                                                    -- Download profile picture
                                                    profilePictureBase64UrlMaybe <- liftIO $ retrieveBase64PictureUrl serverConfiguration serverResources accessToken (picture originalUserInfo)
                                                    case profilePictureBase64UrlMaybe of
                                                        Left msg -> internalServerError $ toResponse ("Unable to fetch profile picture: " `T.append` (T.pack msg))
                                                        Right profilePictureBase64Url -> do
                                                            let userInfo = originalUserInfo { picture = profilePictureBase64Url }
                                                            let userInfoText = TE.decodeUtf8 . BS8.toStrict . A.encode $ userInfo
                                                            -- Save identity token and user info to cookies
                                                            let cookieDuration = (MaxAge $ 30 * 86400)
                                                            addCookies $ (cookieDuration,) <$>
                                                                [ mkCookie (T.unpack $ providerIdentityTokenCookieName knownProvider) $ T.unpack identityTokenText
                                                                , mkCookie (T.unpack $ providerUserInfoCookieName knownProvider) $ T.unpack . encodeUserInfoText $ userInfoText
                                                                ]
                                                            -- Redirect user back to eferer
                                                            redirectToSavedRefererIfAllowed (providerRefererCookieName knownProvider)

readUserIdentityFromCookies :: ServerConfiguration -> ServerResources -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookies serverConfiguration serverResources = do
    resultsForEachProvider <- mapM (readUserIdentityFromCookiesForProvider serverConfiguration serverResources) (knownProviders serverConfiguration)
    return . listToMaybe . catMaybes $ resultsForEachProvider

readUserIdentityFromCookiesForProvider :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> ServerPart (Maybe UserIdentity)
readUserIdentityFromCookiesForProvider serverConfiguration serverResources knownProvider = runMaybeT $ do
    -- Fetch cookie values
    identityTokenText <- lift $ T.pack <$> lookCookieValue (T.unpack $ providerIdentityTokenCookieName knownProvider)
    userInfoText <- MaybeT $ decodeUserInfoText . T.pack <$> lookCookieValue (T.unpack $ providerUserInfoCookieName knownProvider)
    -- Extract claims
    discoveredProvider <- lift $ discoverProvider serverConfiguration serverResources knownProvider
    claims <- MaybeT . liftIO . runMaybeT $ extractClaims serverConfiguration serverResources knownProvider discoveredProvider identityTokenText
    -- Decode user info
    userInfo :: UserInfo <- liftMaybe $ A.decodeStrict (TE.encodeUtf8 userInfoText)
    -- Build response
    let userIdentifier = UserIdentifier ("openid_" `T.append` (knownProviderIdentifier knownProvider)) (sub claims)
    let userPictureUrl = picture userInfo
    let userGivenName = given_name userInfo
    let userFamilyName = family_name userInfo
    return $ UserIdentity userIdentifier userPictureUrl userGivenName userFamilyName

-- * Helper functions
getProviderCallbackUri :: KnownOpenIdProvider -> ServerPart URI
getProviderCallbackUri knownProvider = getCallbackUri $ "/authentication/openid/" `T.append` (knownProviderIdentifier knownProvider) `T.append` "/callback/"

discoverProvider :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> ServerPart OIDC.OIDC
discoverProvider serverConfiguration serverResources knownProvider = do
    let tlsManager = serverResourcesTlsManager serverResources
    provider <- liftIO $ OIDC.discover (knownProviderDiscoveryUrl knownProvider) tlsManager
    let clientId = knownProviderClientId knownProvider
    let clientSecret = knownProviderClientSecret knownProvider
    callbackUri <- getProviderCallbackUri knownProvider
    return $ OIDC.setCredentials (TE.encodeUtf8 clientId) (TE.encodeUtf8 clientSecret) (serializeURIRef' callbackUri) $ OIDC.newOIDC provider

getProviderOAuth2Config :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> OIDC.OIDC -> ServerPart (Either String OA2.OAuth2)
getProviderOAuth2Config serverConfiguration serverResources knownProvider discoveredProvider = do
    let tlsManager = serverResourcesTlsManager serverResources
    let clientId = knownProviderClientId knownProvider
    let clientSecret = knownProviderClientSecret knownProvider
    callbackUri <- getProviderCallbackUri knownProvider
    runExceptT $ do
        authorizeEndpoint <- withExceptT (const "Failed to parse OAuth2 authorization server url.") <$> except <$> parseURI strictURIParserOptions . TE.encodeUtf8 $ OIDCS.oidcAuthorizationServerUrl discoveredProvider
        accessTokenEndpoint <- withExceptT (const "Failed to parse OAuth2 access token endpoint.") <$> except <$> parseURI strictURIParserOptions . TE.encodeUtf8 $ OIDCS.oidcTokenEndpoint discoveredProvider
        return $ OA2.OAuth2
            { OA2.oauthClientId = clientId
            , OA2.oauthClientSecret = (Just clientSecret)
            , OA2.oauthCallback = Just callbackUri
            , OA2.oauthOAuthorizeEndpoint = authorizeEndpoint
            , OA2.oauthAccessTokenEndpoint = accessTokenEndpoint
            }

-- TODO: return Either instead of MaybeT
extractClaims :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> OIDC.OIDC -> T.Text -> MaybeT IO Claims
extractClaims serverConfiguration serverResources knownProvider discoveredProvider identityTokenText = do
    -- Decode jwt token
    let publicKeys = OIDC.jwkSet $ OIDCS.oidcProvider discoveredProvider
    jwtTokenEither <- liftIO $ JWT.decode publicKeys Nothing (TE.encodeUtf8 identityTokenText)
    jwtToken <- liftMaybe $ rightToMaybe jwtTokenEither
    jwsPayload <- liftMaybe $ do
        case jwtToken of
            JWT.Jws (jwsHeader, jwsPayload) -> Just jwsPayload
            _ -> Nothing
    -- Return extracted claims
    liftMaybe $ A.decodeStrict jwsPayload

-- TODO: return IO (Either String T.Text) instead of IO (Maybe T.Text)
fetchUserInfo :: ServerConfiguration -> ServerResources -> KnownOpenIdProvider -> OIDC.OIDC -> OA2.AccessToken -> IO (Maybe T.Text)
fetchUserInfo serverConfiguration serverResources knownProvider discoveredProvider accessToken = do
    let tlsManager = serverResourcesTlsManager serverResources
    let accessTokenText = OA2.atoken accessToken
    let userInfoEndpointMaybe = (OIDC.userinfoEndpoint $ OIDC.configuration $ OIDCS.oidcProvider discoveredProvider)
    case userInfoEndpointMaybe of
        Nothing -> return Nothing
        Just userInfoEndpoint -> do
            initialRequest <- HC.parseRequest (T.unpack userInfoEndpoint)
            let request = initialRequest
                    { HC.method = "POST"
                    , HC.requestHeaders = [("Authorization", TE.encodeUtf8 $ "Bearer " `T.append` accessTokenText)]
                    }
            response <- HC.httpLbs request tlsManager
            return $ Just $ TE.decodeUtf8 . BS8.toStrict $ HC.responseBody response

retrieveBase64PictureUrl :: ServerConfiguration -> ServerResources -> OA2.AccessToken -> T.Text -> IO (Either String T.Text)
retrieveBase64PictureUrl serverConfiguration serverResources accessToken originalPictureUrl = do
    if originalPictureUrl == "https://graph.microsoft.com/v1.0/me/photo/$value" then
        -- Unfortunately, Microsoft Graph's profile picture API no longer works for consumer accounts.
        -- So there is no point in even calling it. Let's just pretend that the user lacks a profile picture.
        return $ Right T.empty
    else do
        let tlsManager = serverResourcesTlsManager serverResources
        let accessTokenText = OA2.atoken accessToken
        initialRequest <- HC.parseRequest (T.unpack originalPictureUrl)
        let request = initialRequest
                { HC.method = "GET"
                , HC.requestHeaders = [("Authorization", TE.encodeUtf8 $ "Bearer " `T.append` accessTokenText)]
                }
        response <- HC.httpLbs request tlsManager
        case (HT.statusCode $ HC.responseStatus response) of
            200 -> error "Not yet implemented!" -- TODO: we should return a base64-encoded image
            404 -> do
                -- If the user lacks a picture, then we just return an empty URL.
                return $ Right T.empty
            _ -> do
                return $ Left "Profile picture for user could not be retrieved."

-- * Cookies
providerCookiePrefix :: KnownOpenIdProvider -> T.Text
providerCookiePrefix knownProvider = (knownProviderIdentifier knownProvider) `T.append` "_"

providerRefererCookieName :: KnownOpenIdProvider -> T.Text
providerRefererCookieName knownProvider = (providerCookiePrefix knownProvider) `T.append` (knownProviderIdentifier knownProvider) `T.append` "_referer"

providerIdentityTokenCookieName :: KnownOpenIdProvider -> T.Text
providerIdentityTokenCookieName knownProvider = (providerCookiePrefix knownProvider) `T.append` (knownProviderIdentifier knownProvider) `T.append` "_identityToken"

providerUserInfoCookieName :: KnownOpenIdProvider -> T.Text
providerUserInfoCookieName knownProvider = (providerCookiePrefix knownProvider) `T.append` (knownProviderIdentifier knownProvider) `T.append` "_userInfo"

-- The cookie for user info needs to be encoded as it contains the character ";", which causes issues in some browsers
encodeUserInfoText :: T.Text -> T.Text
encodeUserInfoText = TE.decodeUtf8 . B64.encode . TE.encodeUtf8

decodeUserInfoText :: T.Text -> Maybe T.Text
decodeUserInfoText = rightToMaybe . fmap TE.decodeUtf8 . B64.decode . TE.encodeUtf8
