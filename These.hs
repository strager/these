import Control.Monad
import Data.Char
import Test.QuickCheck

data These a b = This a | That b | These a b
  deriving (Eq, Ord, Show)

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

testTheseMonadLaws = do
  quickCheck law1
  quickCheck law2
  quickCheck law3
  where
    law1 a b c = (return a >>= k) == k a
      where
        k z = if z then b else c :: These Int Char

    law2 m = (m >>= return) == m
      where _ = (m :: These Int Char)

    law3 m b c d e= (m >>= (\ x -> k x >>= h)) == ((m >>= k) >>= h)
      where
        _ = (m :: These Int Bool)
        k z = if z then b else c :: These Int Char
        h z = if isAlpha z then d else e :: These Int String

test = testTheseMonadLaws
