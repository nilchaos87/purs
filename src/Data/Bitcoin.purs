module Data.Bitcoin (fetchBalance) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)

foreign import deriveAddress :: String -> Int -> String

fetchBalance :: forall e. String -> Aff (ajax :: AJAX | e) Int
fetchBalance = fetchBalance_ 0 0

fetchBalance_ :: forall e. Int -> Int -> String -> Aff (ajax :: AJAX | e) Int
fetchBalance_ acc n addr = do
  xhr <- get $ "https://blockchain.info/q/addressbalance/" <> (deriveAddress addr n)
  let balance = acc + (parseBalance $ Int.fromString xhr.response)
  if balance == acc then (pure balance) else (fetchBalance_ balance (n + 1) addr)

parseBalance :: Maybe Int -> Int
parseBalance Nothing = 0
parseBalance (Just balance) = balance
