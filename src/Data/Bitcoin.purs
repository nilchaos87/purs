module Data.Bitcoin (Wallet, fetchWallet) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Data.Number as Num
import Network.HTTP.Affjax (AJAX, get)

foreign import deriveAddress ∷ String → Int → String

type Wallet = { receiveAddress ∷ String, balance ∷ Number }

fetchWallet ∷ ∀ e. String → Aff (ajax ∷ AJAX | e) Wallet
fetchWallet = fetchWallet_ 0.0 0 0 0

fetchWallet_ ∷ ∀ e. Number → Int → Int → Int → String → Aff (ajax ∷ AJAX | e) Wallet
fetchWallet_ cumulativeBalance missed receiveAddressN n key = do
  let address = deriveAddress key n
  addressBalance ← fetchBalance address
  let miss = addressBalance == 0.0
  let newCumulativeBalance = addressBalance + cumulativeBalance
  let newMissed = if miss then (missed + 1) else 0
  let newReceiveAddressN = if (miss && receiveAddressN == 0) then n else receiveAddressN
  if (newMissed == 5) then (pure $ { receiveAddress: (deriveAddress key receiveAddressN), balance: newCumulativeBalance }) else (fetchWallet_ newCumulativeBalance newMissed newReceiveAddressN (n + 1) key)

fetchBalance ∷ ∀ e. String → Aff (ajax ∷ AJAX | e) Number
fetchBalance address = do
  xhr ← get $ "https://blockchain.info/q/addressbalance/" <> address
  case (Num.fromString xhr.response) of
    Nothing → pure 0.0
    Just n → pure $ n / 100000000.0
