module Counter (mkCounter, mkCounterCtx, ctx) where

import Prelude

import Effect (Effect)
import React.Basic (Context, CreateComponent, component, mkContext, render, toKey, useContext, useEffect, useState, (/\))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

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

mkCounterCtx :: CreateComponent Ctx {}
mkCounterCtx = do
  component "Counter" \props -> do
    counter /\ setCounter <- useState 0

    useEffect [toKey counter] do
      setDocumentTitle $ "Count: " <> show counter
      pure (pure unit)

    cfg <- useContext ctx

    render $ R.button
      { onClick: capture_ $ setCounter (_ + 1)
      , children: [ R.text $ "Increment: " <> show counter, R.text $ "  --  Context: " <> show cfg <> "!!!!" ]
      }

foreign import setDocumentTitle :: String -> Effect Unit      


