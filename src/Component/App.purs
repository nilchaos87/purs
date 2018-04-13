module Component.App (Query(ChangeKey, FetchWallet), component) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Bitcoin (Wallet, fetchWallet)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
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
    render state = walletView state.wallet
      where
        walletView Nothing = HH.text "No wallet to show!"
        walletView (Just { receiveAddress, balance }) =
          HH.div_
            [ HH.img [ HP.src $ "https://api.qrserver.com/v1/create-qr-code/?size=250x250&data=" <> receiveAddress ]
            , HH.div_ [ HH.text receiveAddress ]
            , HH.div_ [ HH.text $ show balance ]
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
