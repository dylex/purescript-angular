module Angular.Promise.Cont
  ( PromiseCont
  , cont
  , cont_
  , cont'_
  ) where

import Data.Either
import Data.Function
import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Control.Monad.Error.Trans

import Angular.Promise

class PromiseCont m where
  cont :: forall e a r. Promise e a -> m (ContT (Promise e r) m a)
  cont_ :: forall e a r. Promise e a -> m (ContT Unit m a)
  cont'_ :: forall e a r. Promise e a -> m (ContT Unit m (Either e a))

foreign import contEff """
  function contEff(promise) {
    return function () {
      return function (k) {
        return function () {
          return promise.then(function (a) {
            return k(a)();
          });
        };
      };
    };
  }""":: forall eff e a r. Promise e a -> Eff eff ((a -> Eff eff (Promise e r)) -> Eff eff (Promise e r))

foreign import contEff_ """
  function contEff_(promise) {
    return function () {
      return function (k) {
        return function () {
          promise.then(function (a) {
            k(a)();
          });
          return {};
        };
      };
    };
  }""":: forall eff e a r. Promise e a -> Eff eff ((a -> Eff eff Unit) -> Eff eff Unit)

foreign import contEff'_ """
  function contEff$prime_(left, right, promise) {
    return function () {
      return function (k) {
        return function () {
          promise.then(function (a) {
            k(right(a))();
          }, function (e) {
            k(left(a))();
          });
          return {};
        };
      };
    };
  }""":: forall eff e a r. Fn3 (e -> Either e a) (a -> Either e a) (Promise e a) (Eff eff ((Either e a -> Eff eff Unit) -> Eff eff Unit))

instance promiseContEff :: PromiseCont (Eff eff) where
  cont p = ContT <$> contEff p
  cont_ p = ContT <$> contEff_ p
  cont'_ p = ContT <$> runFn3 contEff'_ Left Right p
