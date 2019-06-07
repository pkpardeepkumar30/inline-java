{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unsafe.Linear.Extras where

import qualified Linear.Extras as Linear
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe

withLinear :: a ->. (a -> b) ->. b
withLinear = Linear.flip Unsafe.toLinear
