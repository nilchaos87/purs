module Component.App (Query(ChangeAddress), component) where

import Prelude
import Data.Maybe (Maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { address ∷ Maybe String }

data Query a = ChangeAddress (Maybe String) a

type Input = Maybe String

type Message = Void

component ∷ ∀ a. H.Component HH.HTML Query Input Message a
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input ChangeAddress
    }
  where

    initialState ∷ Input → State
    initialState address = { address }

    render ∷ State → H.ComponentHTML Query
    render state = HH.div_ [ HH.text $ show state.address ]

    eval ∷ Query ~> H.ComponentDSL State Query Message a
    eval (ChangeAddress address next) = do
      H.put { address }
      pure next
