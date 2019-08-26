{-# LANGUAGE OverloadedStrings #-}

module Server.Api.V0.Main (handleRoot) where

import Control.Monad (msum)
import Server.Util (forceSlash)
import Happstack.Server

import qualified Data.Aeson as A

handleRoot :: ServerPart Response
handleRoot = msum
    [ forceSlash $ ok . toResponse $ A.encode $ A.object [("success", A.Bool True)]
    ]
