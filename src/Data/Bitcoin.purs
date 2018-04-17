module Data.Bitcoin (Wallet, fetchWallet) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Foldable (sum)
import Data.List (List, (:), drop, filter, fromFoldable, head, length, reverse, take)
import Data.Maybe (Maybe(..))
import Data.Number as Num
import Data.Tuple (Tuple(..), fst, snd)
import Network.HTTP.Affjax (AJAX, get)

foreign import deriveAddress ∷ String → Int → String

type Wallet = { receiveAddress ∷ String, balance ∷ Number }

fetchWallet ∷ ∀ e. String → Aff (ajax ∷ AJAX | e) Wallet
fetchWallet key = do
  addresses ← collectAddresses $ fromFoldable []
  let receiveAddress = findReceiveAddress addresses
  let balance = sum $ snd <$> addresses
  pure $ { receiveAddress, balance }

  where

    findReceiveAddress ∷ List (Tuple String Number) → String
    findReceiveAddress addresses =
      case ((head <<< ((map fst) <<< (filter (((==) 0.0) <<< snd)) <<< reverse)) addresses) of
        Nothing → deriveAddress key 0
        Just val → val

    collectAddresses ∷ List (Tuple String Number) → Aff (ajax ∷ AJAX | e) (List (Tuple String Number))
    collectAddresses addrs = do
      addr ← fetchAddress $ length addrs
      let nextAddrs = addr : addrs
      if ((sum $ snd <$> (take 5 nextAddrs)) == 0.0)
        then (pure $ drop 4 nextAddrs)
        else collectAddresses nextAddrs

    fetchAddress ∷ Int → Aff (ajax ∷ AJAX | e) (Tuple String Number)
    fetchAddress n = do
      let addr = deriveAddress key n
      bal ← fetchBalance addr
      pure $ Tuple addr bal

    fetchBalance ∷ String → Aff (ajax ∷ AJAX | e) Number
    fetchBalance address = do
      xhr ← get $ "https://blockchain.info/q/addressbalance/" <> address
      case (Num.fromString xhr.response) of
        Nothing → pure 0.0
        Just n → pure $ n / 100000000.0
