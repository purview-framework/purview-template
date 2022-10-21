{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (div)

import Purview
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSocket
import qualified Network.Wai.Handler.WebSockets as WaiWebSocket
import Network.HTTP.Types

root = div [ text "hello world" ]

main :: IO ()
main =
  let
    port = 8001
    settings = Warp.setPort port Warp.defaultSettings
  in
   Warp.runSettings settings
     $ WaiWebSocket.websocketsOr
         WebSocket.defaultConnectionOptions
         (webSocketHandler root)
         (httpHandler root)

webSocketHandler component pendingConnection = do
  connection <- WebSocket.acceptRequest pendingConnection
  startWebSocketLoop defaultConfiguration root connection

httpHandler component request respond =
  respond
    $ Wai.responseBuilder
        status200
        [("Content-Type", "text/html")]
        (renderFullPage defaultConfiguration root)
