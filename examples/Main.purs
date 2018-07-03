module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React.Props (onClick)
import RockOn (HUI)
import RockOn.DOM (button, text)
import RockOn.Led (mkLed, turnLedOn)
import RockOn.Run (runWidgetInDom)
import Control.Alternative ((<|>))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import JohnnyFive.Led.FFI (Led)

-- A fixed Led on pin 13
myLed :: Led
myLed = unsafePerformEffect (mkLed 13)

-- Simple Led on off button
ledWidget :: forall a. Boolean -> Widget HUI a
ledWidget ledon = do
  let buttonText = if ledon then "Turn LED Off" else "Turn LED On"
  _ <- button [onClick] [text buttonText] <|> turnLedOn myLed ledon
  ledWidget (not ledon)

main :: Effect Unit
main = runWidgetInDom "hello" (ledWidget false)
