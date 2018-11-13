module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic (element, runContext)
import React.Basic.DOM (render) as DOM
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Counter (mkCounter, mkCounterCtx, ctx)

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c  -> do
      counter <- mkCounter
      let app = element counter {}

      counter2 <- mkCounterCtx
      let app2 = runContext counter2 ctx 100 {}

      DOM.render app2 c



