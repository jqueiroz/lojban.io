module Server.Core where

import Network.HTTP.Client (Manager)

data ServerResources = ServerResources
    { serverResourcesTlsManager :: Manager
    }
