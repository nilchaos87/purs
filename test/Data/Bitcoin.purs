module Test.Data.Bitcoin (main) where

import Prelude
import Control.Monad.Aff.Class (liftAff)
import Network.HTTP.Affjax (AJAX)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.Bitcoin (fetchBalance)

main :: forall e. Spec (ajax :: AJAX | e) Unit
main =
  describe "fetchBalance" do
    it "should get the total balance for the specified xpub address" do
      let xpub = "xpub6CbP8EKGovffnSKQtAYvV44qqCJh9jPBEATfxu78NekYCxRv7MgS55P1yuL1UVfaVkVo9SkvHtBKXUz2rKLsvqemztBUcrP8n1xwqzmLPgx"
      balance <- liftAff $ fetchBalance xpub
      balance `shouldEqual` 39191473
