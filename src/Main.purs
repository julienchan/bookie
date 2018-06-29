module WaiMain where -- TODO: Fix problem of multiple "Main" modules in a client project in a more elegant manner.

import Prelude

import Data.Maybe (Maybe(Nothing))

import Effect (Effect)
import Effect.Console (log)

import Node.HTTP (createServer, listen)
import Network.HTTP.Wai as H
import Network.HTTP.Wai.Run (handleRequest, Application)

simpleApp :: Application
simpleApp _ respond = do
  log $ "Receive request"
  respond $ H.responseStrUtf8 H.status200 [H.contentType "text/plain"] "Hi guys"

main :: Effect Unit
main = do
  server <- createServer (handleRequest simpleApp)
  listen server { hostname: "localhost", port: 2123, backlog: Nothing } $ do
    log "Listening on port 2123."
