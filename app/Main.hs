{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Prelude hiding (div)

import Purview
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSocket
import qualified Network.Wai.Handler.WebSockets as WaiWebSocket
import Network.HTTP.Types

import Debug.Trace

-- trigger = handler [Parent "sideways"] 0 reducer
--   where
--     reducer "up" st = (const 0, [])
--     reducer "down" st = (const 1, [])
--     reducer "incrParent" st = (const 99, [Parent "up"])
--
-- clickHandler = handler [] (0 :: Integer) reducer
--   where
--     reducer "up" st = (const 0, [])
--     reducer "down" st = (const 1, [Self "sideways"])
--     reducer "sideways" st = (const 5, [])
--     reducer _ st = (const 99, [])
--
-- root :: Purview () IO
-- root = clickHandler $ \state -> div
--   [ onClick "up" $ div [ text "up" ]
--   , onClick "down" $ div [ text "down" ]
--   , div [ text (show state) ]
--   , trigger $ \state -> div
--     [ onClick "incrParent" $ div [ text $ "incrParent: " <> show state ]
--     , onClick "down" $ div [ text $ "state: " <> show state ]
--     ]
--   ]

------------------
-- Form Example --
------------------

nameAttr = Attribute . Generic "name"
typeAttr = Attribute . Generic "type"

submitButton = typeAttr "submit" $ button [ text "submit" ]

toString :: Maybe String -> String
toString (Just str) = str
toString (Nothing)  = "nothing"

name state = onSubmit toString $ div
  [ form [ nameAttr "description" $ input []
         , submitButton
         ]
  , text state
  ]

eventHandler = handler [] "" reducer
  where reducer str state = (const "gotcha", [])

root :: Purview () IO
root = eventHandler $ \state -> name state

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
