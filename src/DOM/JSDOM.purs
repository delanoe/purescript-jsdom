module DOM.JSDOM
  ( JSDOM()
  , Callback()
  , env
  , envAff
  , jsdom
  ) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (Error)
-- import Control.Monad.Eff.Uncurried (EffFn2, EffFn4, mkEffFn2, runEffFn2, runEffFn4)
import DOM.HTML.Types (Window)
import DOM.Node.Types (Document)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn2, Fn4, mkFn2, runFn2, runFn4)
import Data.Maybe (maybe)
import Data.Nullable (Nullable, toMaybe)
import Prelude (Unit, ($))

foreign import data JSDOM :: Effect

type JSCallback eff a = Fn2 (Nullable Error) a (Eff (jsdom :: JSDOM | eff) Unit)
type Callback   eff a = Either Error a -> Eff (jsdom :: JSDOM | eff) Unit

toJSCallback :: forall a eff. Callback eff a -> JSCallback eff a
toJSCallback f = mkFn2 (\e a -> f $ maybe (Right a) Left (toMaybe e))

foreign import _jsdom ::
  { env   :: forall configs eff. Fn4 String (Array String) { | configs} (JSCallback eff Window) (Eff (jsdom :: JSDOM | eff) Unit)
  , jsdom :: forall configs eff. Fn2 String { | configs} (Eff (jsdom :: JSDOM | eff) Document)
  }

env :: forall configs eff. String -> Array String -> { | configs} -> Callback eff Window -> (Eff (jsdom :: JSDOM | eff) Unit)
env urlOrHtml scripts configs callback = runFn4 _jsdom.env urlOrHtml scripts configs (toJSCallback callback)

envAff :: forall configs eff. String -> Array String -> { | configs} -> Aff (jsdom :: JSDOM | eff) Window
envAff urlOrHtml scripts configs = makeAff \e a -> env urlOrHtml scripts configs $ either e a

jsdom :: forall configs eff. String -> { | configs} -> Eff (jsdom :: JSDOM | eff) Document
jsdom markup configs = runFn2 _jsdom.jsdom markup configs
