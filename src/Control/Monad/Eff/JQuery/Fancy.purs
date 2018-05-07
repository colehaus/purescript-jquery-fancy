module Control.Monad.Eff.JQuery.Fancy
  ( module Control.Monad.Eff.JQuery.Fancy
  , module ForReExport
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (Selector)
import Control.Monad.Eff.JQuery as J
import DOM (DOM)
import DOM.Event.Types (EventType)
import Data.Array as Array
import Data.Foldable (length)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

import Control.Monad.Eff.JQuery.Fancy.Internal (JQuery(MkJQuery), One, widthImpl)
import Control.Monad.Eff.JQuery.Fancy.Internal (None, One, Many, JQuery) as ForReExport

width :: forall e tag. JQuery (One tag) -> Eff (dom :: DOM | e) Number
width = widthImpl <<< unwrap

clearOne :: forall e tag. JQuery (One tag) -> Eff (dom :: DOM | e) Unit
clearOne = J.clear <<< unwrap

getProp :: forall e tag. String -> JQuery (One tag) -> Eff ( dom :: DOM | e) Foreign
getProp s = J.getProp s <<< unwrap

selectOne ::
     forall e tag.
     Selector
  -> Eff (dom :: DOM | e) (Maybe (JQuery (One tag)))
selectOne s = do
  els <- J.toArray =<< J.select s
  pure case Tuple (Array.head els) (length els) of
    Tuple (Just el) 1 -> Just <<< MkJQuery $ el
    _ -> Nothing

selectOnes ::
     forall e tag.
     Selector
  -> Eff (dom :: DOM | e) (Array (JQuery (One tag)))
selectOnes = map (map MkJQuery) <<< J.toArray <=< J.select

unsafeSelectOneBecause ::
     forall e tag.
     String
  -> Selector
  -> Eff (dom :: DOM | e) (JQuery (One tag))
unsafeSelectOneBecause err = map fromJust <<< selectOne
  where
    fromJust (Just x) = x
    fromJust Nothing = unsafeCrashWith err

on :: forall e q.
     EventType
  -> (J.JQueryEvent -> JQuery q -> Eff (dom :: DOM | e) Unit)
  -> JQuery q
  -> Eff (dom :: DOM | e) Unit
on eventType handler =
  J.on (unwrap eventType) (\e q -> handler e (MkJQuery q)) <<< unwrap

getValue :: forall e tag. JQuery (One tag) -> Eff (dom :: DOM | e) Foreign
getValue = J.getValue <<< unwrap

setText :: forall e tag. String -> JQuery (One tag) -> Eff (dom :: DOM | e) Unit
setText s = J.setText s <<< unwrap

getText :: forall e tag. JQuery (One tag) -> Eff (dom :: DOM | e) String
getText = J.getText <<< unwrap
