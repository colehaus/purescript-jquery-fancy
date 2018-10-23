module Control.Monad.Eff.JQuery.Fancy.Internal where

import Data.Newtype (class Newtype)
import Effect (Effect)
import JQuery as J

foreign import kind Quantity
foreign import data None :: Quantity
foreign import data One :: Symbol -> Quantity
foreign import data Many :: Quantity

newtype JQuery (q :: Quantity) = MkJQuery J.JQuery
derive instance newtypeJQuery :: Newtype (JQuery q) _

foreign import widthImpl :: J.JQuery -> Effect Number

