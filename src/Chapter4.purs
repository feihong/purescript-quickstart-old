module Chapter4 where
  
import Control.MonadZero (guard)
import Data.Array (filter, length, cons, (:), (..))
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Path (Path, filename, isDirectory, ls, root, size)
import Partial.Unsafe (unsafePartial)
import Prelude

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

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\acc v -> acc && v) true

mysteryFunction :: Array Boolean -> Boolean
mysteryFunction = foldl (==) false

-- Rewrite this function in tail recursive form
badCount :: forall a. (a -> Boolean) -> Array a -> Int
badCount _ [] = 0
badCount p xs = 
  if p (unsafePartial head xs) 
    then badCount p (unsafePartial tail xs) + 1
    else badCount p (unsafePartial tail xs)

count :: forall a. (a -> Boolean) -> Array a -> Int
count p = count' 0
  where 
    count' v [] = v
    count' v xs = 
      let v' = if p (unsafePartial head xs) then v + 1 else v
      in count' v' (unsafePartial tail xs)

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc n -> cons n acc) []

allFiles :: Path -> Array Path
allFiles path = path : do
  child <- ls path
  allFiles child

onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter (isDirectory >>> not)

largestAndSmallest :: { smallest :: Maybe Path, largest :: Maybe Path }
largestAndSmallest = 
  (onlyFiles >>> foldl inner {largest: Nothing,  smallest: Nothing}) root
  where 
    inner {largest, smallest} file = 
      let l' = maybe file (maxFile file) largest
          s' = maybe file (minFile file) smallest
      in {largest: Just l', smallest: Just s'}
    maxFile p1 p2 = 
      case [size p1, size p2] of
        [Just s1, Just s2] -> if s1 > s2 then p1 else p2
        _ -> p1   -- unreachable
    minFile p1 p2 = 
      case [size p1, size p2] of
        [Just s1, Just s2] -> if s1 < s2 then p1 else p2
        _ -> p1   -- unreachable

whereIs' :: String -> Path -> Array Path
whereIs' name path = do
  child <- ls path
  if filename child == name 
    then pure child
    else whereIs' name child

whereIs  :: String -> Maybe Path
whereIs name = 
  case whereIs' name root of
    [] -> Nothing
    x -> Just $ unsafePartial head x