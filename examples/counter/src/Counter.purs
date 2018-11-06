module Counter where

import Prelude

import React.Basic (Component, JSX, StateUpdate(..), capture_, createComponent, make)
import React.Basic.DOM as R

component :: Component Props
component = createComponent "Counter"

type Props =
  { label :: String
  }

data Action
  = Increment

counter :: Props -> JSX
counter = make component { initialState, update, render }
  where
    initialState = { counter: 0 }

    update self = case _ of
      Increment ->
        Update self.state { counter = self.state.counter + 1 }

    render self =
      R.button
        { onClick: capture_ self Increment
        , children: [ R.text (self.props.label <> ": " <> show self.state.counter) ]
        }
