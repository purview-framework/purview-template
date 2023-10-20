{-# LANGUAGE QuasiQuotes #-}
module Main where

import Prelude hiding (div)

import Purview
import Purview.Server

{-

This is a short example to get started with.

When you trigger a blur on one field, it sends the text
to the event handler.

The same thing happens when you trigger change on the other
text field.

We just add something to the front of the text, and the new
state flows back down for display.

-}

-- some helpers
typeAttr = Attribute . Generic "type"
nameAttr = Attribute . Generic "name"

textField = nameAttr "text" $ typeAttr "text" $ input []

-- events we can handle
data Event = Blur String | Change String
  deriving (Show, Eq)

-- these take the incoming data and turn them into events
-- we can handle
toBlur :: Maybe String -> Event
toBlur = maybe (Blur "") Blur

toChange :: Maybe String -> Event
toChange = maybe (Change "") Change

viewStyle = [style|
  font-size: 24px;
  div {
    margin: 20px;
  }
|]

view :: String -> Purview Event m
view state = viewStyle $ div
  [ div [ text $ "The current state: " <> state ]
  , div [ onBlur toBlur textField ]
  , div [ onChange toChange textField ]
  ]

eventsHandler = handler' [] "" reducer
  where
    reducer (Blur str)   state = ("blur: " <> str, [])
    reducer (Change str) state = ("change: " <> str, [])

root url = eventsHandler view

main :: IO ()
main = serve defaultConfiguration root
