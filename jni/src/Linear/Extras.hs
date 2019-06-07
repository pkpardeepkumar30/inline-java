{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Linear.Extras where

import Control.Monad.Linear
import qualified System.IO
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe

-- | Monadic fold over the elements of a list,
-- associating to the left, i.e. from left to right.
foldM :: forall m a b. Monad m => (b ->. a ->. m b) -> b ->. [a] ->. m b
foldM f z0 xs = foldr f' return xs z0
  where
    f' :: a ->. (b ->. m b) ->. b ->. m b
    f' x k z = f z x >>= k

foldr :: (a ->. b ->. b) -> b ->. [a] ->. b
foldr f z = \case
  [] -> z
  x:xs -> f x (foldr f z xs)

flip :: (a -->.(p) b -->.(q) c) -->.(r) b -->.(q) a -->.(p) c
flip f b a = f a b

(.) :: (b ->. c) ->. (a ->. b) ->. a ->. c
(f . g) x = f (g x)

toSystemIO :: Linear.IO a ->. System.IO.IO a
toSystemIO = Unsafe.coerce
