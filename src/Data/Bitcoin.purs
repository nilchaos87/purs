module Data.Bitcoin (Wallet, fetchWallet) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)

foreign import deriveAddress ∷ String → Int → String

type Wallet = { receiveAddress ∷ String, balance ∷ Int }

fetchWallet ∷ ∀ e. String → Aff (ajax ∷ AJAX | e) Wallet
fetchWallet = fetchWallet_ 0 0 0 0

fetchWallet_ ∷ ∀ e. Int → Int → Int → Int → String → Aff (ajax ∷ AJAX | e) Wallet
fetchWallet_ misses balance lastHitN n key = do
  xhr ← get $ "https://blockchain.info/q/addressbalance/" <> currentAddress
  let currentBalance = balanceFromString xhr.response
  let newBalance = balance + currentBalance
  let miss = currentBalance == 0
  let newMisses = if miss then (misses + 1) else 0
  let newLastHitN = if miss then lastHitN else n
  if (newMisses == 5) then (pure $ wallet currentBalance (lastHitN + 2)) else (fetchWallet_ newMisses newBalance newLastHitN (n + 1) key)
  where
    currentAddress = deriveAddress key n
    wallet bal nohitN = { receiveAddress: (deriveAddress key nohitN), balance: bal }

balanceFromString ∷ String → Int
balanceFromString str = case (Int.fromString str) of
  Nothing → 0
  Just b → b
