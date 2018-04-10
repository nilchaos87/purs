module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Network.HTTP.Affjax (AJAX)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Test.Data.Bitcoin as Bitcoin

main :: Eff (RunnerEffects (ajax :: AJAX)) Unit
main = run [consoleReporter] Bitcoin.main
