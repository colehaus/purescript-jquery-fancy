module Control.Monad.Eff.JQuery.Fancy.Internal where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery as J
import DOM (DOM)
import Data.Newtype (class Newtype)

foreign import kind Quantity
foreign import data None :: Quantity
foreign import data One :: Symbol -> Quantity
foreign import data Many :: Quantity

newtype JQuery (q :: Quantity) = MkJQuery J.JQuery
derive instance newtypeJQuery :: Newtype (JQuery q) _

foreign import widthImpl :: forall e. J.JQuery -> Eff (dom :: DOM | e) Number

