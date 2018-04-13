module Main where

import Prelude
import Control.Coroutine (Consumer, Producer, ($$), consumer, runProcess)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.String as Str
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (hashchange)
import DOM.HTML.Event.HashChangeEvent (newURL)
import DOM.HTML.Event.Types (HashChangeEvent, readHashChangeEvent)
import DOM.HTML.Location (hash)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Window (location)
import Halogen (action)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Component.App as App

main ∷ Eff (HalogenEffects (ajax ∷ AJAX)) Unit
main = runHalogenAff $ do
  body ← awaitBody
  key ← liftEff getKey
  io ← runUI App.component key body
  io.query $ action $ App.ChangeKey key
  runProcess (hashChangeProducer $$ hashChangeConsumer io.query)

keyFromHash ∷ String → Maybe String
keyFromHash "" = Nothing
keyFromHash "#" = Nothing
keyFromHash h = Just $ Str.drop 1 h

getKey ∷ ∀ e. Eff (dom ∷ DOM | e) (Maybe String)
getKey = keyFromHash <$> (window >>= location >>= hash)

hashChangeConsumer ∷ ∀ e. (App.Query ~> Aff (HalogenEffects e)) → Consumer HashChangeEvent (Aff (HalogenEffects e)) Unit
hashChangeConsumer query = consumer \event → do
  let address = keyFromHash $ Str.dropWhile (_ /= '#') $ newURL event
  query $ action $ App.ChangeKey address
  pure Nothing

hashChangeProducer ∷ ∀ e. Producer HashChangeEvent (Aff (avar ∷ AVAR, dom ∷ DOM | e)) Unit
hashChangeProducer = produce \emit →
  let
    emitter e =
      case runExcept (readHashChangeEvent (toForeign e)) of
        Left _ → pure unit
        Right hce → emit (Left hce)
  in
    liftEff $
      window
        >>= windowToEventTarget
        >>> addEventListener hashchange (eventListener emitter) false
