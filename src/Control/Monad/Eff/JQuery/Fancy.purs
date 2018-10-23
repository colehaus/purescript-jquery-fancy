module Control.Monad.Eff.JQuery.Fancy
  ( module Control.Monad.Eff.JQuery.Fancy
  , module ForReExport
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (length)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign (Foreign)
import JQuery (Selector)
import JQuery as J
import Partial.Unsafe (unsafeCrashWith)
import Web.Event.Event (EventType)

import Control.Monad.Eff.JQuery.Fancy.Internal (JQuery(MkJQuery), Many, One, widthImpl)
import Control.Monad.Eff.JQuery.Fancy.Internal (None, One, Many, JQuery) as ForReExport


width :: forall tag. JQuery (One tag) -> Effect Number
width = widthImpl <<< unwrap

clearOne :: forall tag. JQuery (One tag) -> Effect Unit
clearOne = J.clear <<< unwrap

getProp :: forall tag. String -> JQuery (One tag) -> Effect Foreign
getProp s = J.getProp s <<< unwrap

select ::
     Selector
  -> Effect (Maybe (JQuery Many))
select s = do
  j <- J.select s
  els <- J.toArray j
  pure $ if length els > 0 then Just (MkJQuery j) else Nothing

selectOne ::
     forall tag.
     Selector
  -> Effect (Maybe (JQuery (One tag)))
selectOne s = do
  els <- J.toArray =<< J.select s
  pure case Tuple (Array.head els) (length els) of
    Tuple (Just el) 1 -> Just <<< MkJQuery $ el
    _ -> Nothing

selectOnes ::
     forall tag.
     Selector
  -> Effect (Array (JQuery (One tag)))
selectOnes = map (map MkJQuery) <<< J.toArray <=< J.select

unsafeSelectOneBecause ::
     forall tag.
     String
  -> Selector
  -> Effect (JQuery (One tag))
unsafeSelectOneBecause err = map fromJust <<< selectOne
  where
    fromJust (Just x) = x
    fromJust Nothing = unsafeCrashWith err

on :: forall q.
     EventType
  -> (J.JQueryEvent -> JQuery q -> Effect Unit)
  -> JQuery q
  -> Effect Unit
on eventType handler =
  J.on (unwrap eventType) (\e q -> handler e (MkJQuery q)) <<< unwrap

getValue :: forall tag. JQuery (One tag) -> Effect Foreign
getValue = J.getValue <<< unwrap

setText :: forall tag. String -> JQuery (One tag) -> Effect Unit
setText s = J.setText s <<< unwrap

getText :: forall tag. JQuery (One tag) -> Effect String
getText = J.getText <<< unwrap

hide :: JQuery Many -> Effect Unit
hide = J.hide <<< unwrap

hideOne :: forall tag. JQuery (One tag) -> Effect Unit
hideOne = J.hide <<< unwrap

displayOne :: forall tag. JQuery (One tag) -> Effect Unit
displayOne = J.display <<< unwrap

setVisibleOne :: forall tag. Boolean -> JQuery (One tag) -> Effect Unit
setVisibleOne b = J.setVisible b <<< unwrap

toggleOne :: forall tag. JQuery (One tag) -> Effect Unit
toggleOne = J.toggle <<< unwrap
