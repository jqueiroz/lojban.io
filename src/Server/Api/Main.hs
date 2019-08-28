{-# LANGUAGE OverloadedStrings #-}

module Server.Api.Main (handleRoot) where

import Control.Monad (msum)
import Happstack.Server
import qualified Server.Api.V0.Main as V0

handleRoot :: ServerPart Response
handleRoot = msum
    [ dir "v0" $ V0.handleRoot
    ]
