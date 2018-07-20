module Chapter4 where
  
import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, length, (..))
import Data.Array.Partial (head, tail)
-- import Data.List (List)
import Partial.Unsafe (unsafePartial)

isEven :: Int -> Boolean
isEven 0 = true
isEven x  = 
  if x > 0 then
    isEven $ x - 1
  else
    isEven $ x + 1

countEven :: Array Int -> Int
countEven [] = 0
countEven arr = 
  let x = unsafePartial head arr
      v = if (x `mod` 2 == 0) then 1 else 0
  in v + countEven (unsafePartial tail arr)

squares :: Array Number -> Array Number
squares = map (\n -> n * n)

removeNegatives :: Array Number -> Array Number
removeNegatives = filter (\n -> n >= 0.0)

infixl 4 filter as <$?>

removeNegatives' :: Array Number -> Array Number
removeNegatives' arr = (\n -> n >= 0.0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime = factors >>> length >>> (\n -> n <= 1)

cartesianProduct  :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  let n' = n - 1
  x <- 1 .. n'
  y <- x .. n'
  z <- y .. n'
  guard $ x*x + y*y == z*z
  pure [x, y, z]

-- Skip factorizations problem, don't understand
