module Test.Data.Bitcoin (main) where

import Prelude
import Control.Monad.Aff.Class (liftAff)
import Network.HTTP.Affjax (AJAX)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Bitcoin (fetchWallet)

main :: forall e. Spec (ajax :: AJAX | e) Unit
main =
  describe "fetchWallet" do
    it "should get the receive address for the wallet" do
      let xpub = "xpub6CbP8EKGovffnSKQtAYvV44qqCJh9jPBEATfxu78NekYCxRv7MgS55P1yuL1UVfaVkVo9SkvHtBKXUz2rKLsvqemztBUcrP8n1xwqzmLPgx"
      wallet <- liftAff $ fetchWallet xpub
      wallet.receiveAddress `shouldEqual` "33F7Ab1e7t82u8ZY8hCVFU8YDHXJQwF4nF"
    it "should get the total balance for the wallet" do
      let xpub = "xpub6CbP8EKGovffnSKQtAYvV44qqCJh9jPBEATfxu78NekYCxRv7MgS55P1yuL1UVfaVkVo9SkvHtBKXUz2rKLsvqemztBUcrP8n1xwqzmLPgx"
      wallet <- liftAff $ fetchWallet xpub
      wallet.balance `shouldEqual` 0.3940587
