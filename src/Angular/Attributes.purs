module Angular.Attributes
  ( Attributes()
  , NgAttr()
  , AttrEff()
  , addClass
  , removeClass
  , updateClass
  , observe
  , set
  , get
  , attr
  ) where

import Control.Monad.Eff

foreign import data Attributes :: *

foreign import data NgAttr :: !

type AttrEff e r = Eff (ngattr :: NgAttr | e) r

foreign import addClass
  " function addClass(classVal){ \
  \   return function(attrs){ \
  \     return function(){ \
  \       return attrs.$addClass(classVal); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Attributes -> AttrEff e Unit

foreign import removeClass
  " function removeClass(classVal){ \
  \   return function(attrs){ \
  \     return function(){ \
  \       return attrs.$removeClass(classVal); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Attributes -> AttrEff e Unit

foreign import updateClass
  " function updateClass(newClasses){ \
  \   return function(oldClasses){ \
  \     return function(attrs){ \
  \       return function(){ \
  \         return attrs.$updateClass(newClasses, oldClasses); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> String -> Attributes -> AttrEff e Unit

foreign import observe
  " function observe(key){ \
  \   return function(k){ \
  \     return function(attrs){ \
  \       return function(){ \
  \         return attrs.$observe(key, function(v){return k(v)();}); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e f. String -> (String -> Eff f Unit) -> Attributes -> AttrEff e Unit

foreign import set
  " function set(name){ \
  \   return function(value){ \
  \     return function(attrs){ \
  \       return function(){ \
  \         return attrs.$set(name, value); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> String -> Attributes -> AttrEff e Unit

foreign import get
  " function get(attrs){ \
  \   return function(){ \
  \     return attrs; \
  \   }; \
  \ }"
  :: forall e a. Attributes -> AttrEff e { | a }

foreign import attr
  " function attr(attrs){ \
  \   return function(){ \
  \     return attrs.$attr; \
  \   }; \
  \ }"
  :: forall e a. Attributes -> AttrEff e { | a }
