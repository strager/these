import Data.Char
import Test.QuickCheck

import Data.These

testTheseFunctorLaws = do
  quickCheck law1
  quickCheck law2

  where
    law1 x = fmap id x == x
      where _ = (x :: These Int Char)

    law2 x = fmap (f . g) x == fmap f (fmap g x)
      where
        _ = (x :: These Int Char)

        f :: String -> Int
        f = length

        g :: Char -> String
        g = show

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

test = testTheseMonadLaws >> testTheseFunctorLaws
