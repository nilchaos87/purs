module Component.App (Query(ChangeKey), component) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Bitcoin (Wallet, fetchWallet)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax (AJAX)

type State = { key ∷ Maybe String, wallet ∷ Maybe Wallet }

data Query a = ChangeKey (Maybe String) a

type Input = Maybe String

type Message = Void

component ∷ ∀ a. H.Component HH.HTML Query Input Message (Aff (ajax ∷ AJAX | a))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input ChangeKey
    }
  where

    initialState ∷ State
    initialState = { key: Nothing, wallet: Nothing }

    render ∷ State → H.ComponentHTML Query
    render state = HH.div_ [ HH.text $ displayWallet state.wallet ]
      where
        displayWallet Nothing = "--"
        displayWallet (Just wallet) = wallet.receiveAddress <> ": " <> (show wallet.balance)

    eval ∷ Query ~> H.ComponentDSL State Query Message (Aff (ajax ∷ AJAX | a))
    eval (ChangeKey key next) = do
      H.put { key, wallet: Nothing }
      case key of
        Nothing →
          pure next
        Just k → do
          wallet ← H.liftAff $ fetchWallet k
          H.put { key, wallet: Just wallet }
          pure next
