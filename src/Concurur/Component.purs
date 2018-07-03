module Concurur.Component where

import Prelude

import Data.Traversable (traverse_)
import Effect (Effect)
import JohnnyFive.Led.FFI as Led

-- Only support on off states for the time being
newtype HWComponent
  = Led
    { led :: Led.Led
    , on :: Boolean
    }

type HWComponents = Array HWComponent

-- TODO: Simplest (AND buggy) implementation, needs to be changed
runHW :: HWComponents -> HWComponents -> Effect Unit
runHW _old new = traverse_ runLed new
  where
    runLed (Led led) = if led.on
      then Led.on led.led
      else Led.off led.led
