module Component.App (Query(ChangeKey, FetchWallet), Message(KeyChanged), component) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Bitcoin (Wallet, fetchWallet)
import Data.Array ((:))
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)

type State = { key ∷ Maybe String, pendingKey ∷ String, wallet ∷ Maybe Wallet }

data Query a
  = ChangeKey (Maybe String) a
  | ChangePendingKey String a
  | SubmitPendingKey a
  | FetchWallet a
  | Reset a

type Input = Maybe String

data Message = KeyChanged (Maybe String)

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
    initialState key = { key, pendingKey: "", wallet: Nothing }

    render ∷ State → H.ComponentHTML Query
    render { key, pendingKey, wallet } = HH.div [ HP.class_ $ ClassName "app" ] $ header : content
      where
        header = HH.header_
          [ fromMaybe (HH.span_ []) leftAction
          , HH.h1_ [ HH.text "Purs" ]
          , fromMaybe (HH.span_ []) rightAction
          ]
        leftAction =
          case wallet of
            Nothing →
              Nothing
            Just _ →
              Just $ HH.i
                [ HP.class_ $ ClassName "fa fa-refresh"
                , HE.onClick $ HE.input_ FetchWallet
                ]
                []
        rightAction =
          case wallet of
            Just _ →
              Just $ HH.i
                [ HP.class_ $ ClassName "fa fa-times"
                , HE.onClick $ HE.input_ Reset
                ]
                []
            Nothing →
              if pendingKey == ""
                then Nothing
                else (Just $ HH.i
                        [ HP.class_ $ ClassName "fa fa-check"
                        , HE.onClick $ HE.input_ SubmitPendingKey
                        ]
                        [])
        content =
          case wallet of
            Nothing →
              case key of
                Nothing →
                  [ HH.div [ HP.class_ $ ClassName "no-key" ]
                      [ HH.input [ HE.onValueInput $ HE.input ChangePendingKey ] ]
                  ]
                Just _ →
                  [ HH.div [ HP.class_ $ ClassName "loading" ]
                      [ HH.i [ HP.class_ $ ClassName "fa fa-spinner fa-spin" ] []
                      ]
                  ]
            Just { receiveAddress, balance } →
              [ HH.div [ HP.class_ $ ClassName "balance" ]
                  [ HH.div [ HP.class_ $ ClassName "label" ] [ HH.text "Current Balance" ]
                  , HH.div [ HP.class_ $ ClassName "value" ] [ HH.text $ show balance ]
                  ]
              , HH.div [ HP.class_ $ ClassName "receive-address-qrcode" ]
                  [ HH.img
                      [ HP.src $ "https://api.qrserver.com/v1/create-qr-code/?size=500x500&data=" <> receiveAddress ]
                  ]
              , HH.div [ HP.class_ $ ClassName "receive-address-text" ] [ HH.text receiveAddress ]
              ]

    eval ∷ Query ~> H.ComponentDSL State Query Message (Aff (ajax ∷ AJAX | a))
    eval (ChangeKey key next) = do
      H.put { key, pendingKey: "", wallet: Nothing }
      H.raise $ KeyChanged key
      pure next
    eval (ChangePendingKey pendingKey next) = do
      key ← H.gets _.key
      H.put { key, pendingKey, wallet: Nothing }
      pure next
    eval (SubmitPendingKey next) = do
      pendingKey ← H.gets _.pendingKey
      H.put { key: Just pendingKey, pendingKey: "", wallet: Nothing }
      H.raise $ KeyChanged $ Just pendingKey
      eval $ FetchWallet next
    eval (FetchWallet next) = do
      key ← H.gets _.key
      H.put { key, pendingKey: "", wallet: Nothing }
      case key of
        Nothing →
          pure next
        Just k → do
          wallet ← H.liftAff $ fetchWallet k
          H.put { key, pendingKey: "", wallet: Just wallet }
          pure next
    eval (Reset next) = do
      H.put { key: Nothing, pendingKey: "", wallet: Nothing }
      H.raise $ KeyChanged Nothing
      pure next
