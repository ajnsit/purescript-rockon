module Concurur.Run where

import Prelude

import Concur.Core (Widget)
import Concurur (HUI, renderComponent)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import ReactDOM as ReactDOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

runWidgetInDom :: forall a. String -> Widget HUI a -> Effect Unit
runWidgetInDom elemId w = do
  win <- DOM.window
  doc <- DOM.document win
  let node = DOM.toNonElementParentNode doc
  mroot <- DOM.getElementById elemId node
  case mroot of
    Nothing -> pure unit
    Just root -> void (ReactDOM.render (renderComponent w) root)
