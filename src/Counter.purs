module Counter where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic (Context, CreateComponent, component, element, mkContext, render, runContext, toKey, useContext, useEffect, useState, (/\))
import React.Basic.DOM (render) as DOM
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Ctx = Int 

ctx :: Context Ctx 
ctx = mkContext 5

mkCounter :: ∀ ctx. CreateComponent ctx {}
mkCounter = do
  component "Counter" \props -> do
    counter /\ setCounter <- useState 0

    useEffect [toKey counter] do
      setDocumentTitle $ "Count: " <> show counter
      pure (pure unit)

    render $ R.button
      { onClick: capture_ $ setCounter (_ + 1)
      , children: [ R.text $ "Increment: " <> show counter ]
      }

mkCounter2 :: CreateComponent Ctx {}
mkCounter2 = do
  component "Counter" \props -> do
    counter /\ setCounter <- useState 0

    useEffect [toKey counter] do
      setDocumentTitle $ "Count: " <> show counter
      pure (pure unit)

    cfg <- useContext ctx  

    render $ R.button
      { onClick: capture_ $ setCounter (_ + 1)
      , children: [ R.text $ "Increment: " <> show counter, R.text $ "  --  Context: " <> show cfg ]
      }

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c  -> do
      counter <- mkCounter
      let app = element counter {}

      counter2 <- mkCounter2
      let app2 = runContext counter2 ctx 10 {}

      DOM.render app2 c


foreign import setDocumentTitle :: String -> Effect Unit      


