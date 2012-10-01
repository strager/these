module These where

import Control.Monad
import Test.QuickCheck

data These a b = This a | That b | These a b
  deriving (Eq, Ord, Show)

instance Functor (These e) where
  fmap _ (This e) = This e
  fmap f (That a) = That (f a)
  fmap f (These e a) = These e (f a)

instance Monad (These e) where
  return = That

  (This e) >>= _ = This e
  (That a) >>= f = f a
  (These e a) >>= f
    = case f a of
      This _ -> This e
      That a' -> These e a'
      These _ a' -> These e a'

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = oneof
    [ liftM This arbitrary
    , liftM That arbitrary
    , liftM2 These arbitrary arbitrary
    ]
