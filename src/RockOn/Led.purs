module RockOn.Led where

import Concur.Core (Widget)
import RockOn (HUI, hwWidget)
import RockOn.Component (HWComponent(..))
import Effect (Effect)
import JohnnyFive.Led.FFI (Led)
import JohnnyFive.Led.FFI as Led

mkLed :: Int -> Effect Led
mkLed = Led.mkLed

turnLedOn :: forall a. Led -> Boolean -> Widget HUI a
turnLedOn led on = hwWidget (Led {led: led, on: on})
