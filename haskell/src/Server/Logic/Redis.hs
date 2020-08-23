{-# LANGUAGE OverloadedStrings #-}

module Server.Logic.Redis
( encodeRedisKey
, runRedis
) where

import Server.Core
import qualified Data.Text as T
import qualified Database.Redis as Redis

encodeRedisKey :: [(T.Text, T.Text)] -> T.Text
encodeRedisKey = T.concat . map encodeIdentifier where
    encodeIdentifier :: (T.Text, T.Text) -> T.Text
    encodeIdentifier (key, value) = T.concat [ "[", key, "=\"", T.replace "\"" "\"\"" value, "\"]" ]

runRedis :: ServerConfiguration -> ServerResources -> Redis.Redis a -> IO a
runRedis serverConfiguration serverResources redis = do
    let conn = serverResourcesRedisConnection serverResources
    Redis.runRedis conn redis
