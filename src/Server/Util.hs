{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Server.Util where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import qualified Data.ByteString.Lazy as BS

forceSlash :: ServerPart Response -> ServerPart Response
forceSlash x = nullDir >> msum [trailingSlash >> x, askRq >>= \rq -> seeOther (rqUri rq ++ "/") (toResponse ())]

getBody :: ServerPart BS.ByteString
getBody = askRq >>= liftIO . takeRequestBody >>=
    \case
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return ""
