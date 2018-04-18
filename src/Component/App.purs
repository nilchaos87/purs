module Component.App (Query(ChangeKey, FetchWallet), component) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Bitcoin (Wallet, fetchWallet)
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)

type State = { key ∷ Maybe String, wallet ∷ Maybe Wallet }

data Query a = ChangeKey (Maybe String) a | FetchWallet a

type Input = Maybe String

type Message = Void

component ∷ ∀ a. H.Component HH.HTML Query Input Message (Aff (ajax ∷ AJAX | a))
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: HE.input ChangeKey
    , initializer: Just $ H.action FetchWallet
    , finalizer: Nothing
    }
  where

    initialState ∷ Input → State
    initialState key = { key, wallet: Nothing }

    render ∷ State → H.ComponentHTML Query
    render { key, wallet } = HH.div [ HP.class_ $ ClassName "app" ] $ (HH.header_ [ HH.text "Purs" ]) : content
      where
        content =
          case wallet of
            Nothing →
              case key of
                Nothing →
                  [ HH.div [ HP.class_ $ ClassName "no-key" ] [ HH.text "No key selected" ] ]
                Just _ →
                  [ HH.div [ HP.class_ $ ClassName "loading" ]
                      [ HH.div [ HP.class_ $ ClassName "indicator" ] []
                      ]
                  ]
            Just { receiveAddress, balance } →
              [ HH.div [ HP.class_ $ ClassName "balance" ]
                  [ HH.div [ HP.class_ $ ClassName "label" ] [ HH.text "Current Balance" ]
                  , HH.div [ HP.class_ $ ClassName "value" ] [ HH.text $ show balance ]
                  ]
              , HH.div [ HP.class_ $ ClassName "receive-address-qrcode" ]
                  [ HH.img [ HP.src $ "https://api.qrserver.com/v1/create-qr-code/?size=500x500&data=" <> receiveAddress ]
                  ]
              , HH.div [ HP.class_ $ ClassName "receive-address-text" ] [ HH.text receiveAddress ]
              ]

    eval ∷ Query ~> H.ComponentDSL State Query Message (Aff (ajax ∷ AJAX | a))
    eval (ChangeKey key next) = do
      H.put { key, wallet: Nothing }
      pure next
    eval (FetchWallet next) = do
      key ← H.gets _.key
      case key of
        Nothing →
          pure next
        Just k → do
          wallet ← H.liftAff $ fetchWallet k
          H.put { key, wallet: Just wallet }
          pure next
