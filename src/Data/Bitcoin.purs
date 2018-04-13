module Data.Bitcoin (Wallet, fetchWallet) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)

foreign import deriveAddress ∷ String → Int → String

type Wallet = { receiveAddress ∷ String, balance ∷ Int }

fetchWallet ∷ ∀ e. String → Aff (ajax ∷ AJAX | e) Wallet
fetchWallet = fetchWallet_ 0 0

fetchWallet_ ∷ ∀ e. Int → Int → String → Aff (ajax ∷ AJAX | e) Wallet
fetchWallet_ balance n key = do
  xhr ← get $ "https://blockchain.info/q/addressbalance/" <> receiveAddress
  let additionalBalance = balanceFromString xhr.response
  if (additionalBalance == 0) then (pure $ currentWallet) else (fetchWallet_ (balance + additionalBalance) (n + 1) key)
  where
    receiveAddress = deriveAddress key n
    currentWallet = { receiveAddress, balance }

balanceFromString ∷ String → Int
balanceFromString str = case (Int.fromString str) of
  Nothing → 0
  Just b → b
