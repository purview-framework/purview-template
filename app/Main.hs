{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (div)

import Purview
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSocket
import qualified Network.Wai.Handler.WebSockets as WaiWebSocket
import Network.HTTP.Types

clickHandler = handler 0 reducer
  where
    reducer "up" st = (const 0, [])
    reducer "down" st = (const 1, [])

root = clickHandler $ \state -> div
  [ onClick "up" $ div [ text "up" ]
  , onClick "down" $ div [ text "down" ]
  , div [ text (show state) ]
  ]

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
  startWebSocketLoop defaultConfiguration { devMode=True } root connection

httpHandler component request respond =
  respond
    $ Wai.responseBuilder
        status200
        [("Content-Type", "text/html")]
        (renderFullPage defaultConfiguration root)
