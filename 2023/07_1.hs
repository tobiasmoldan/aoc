import Data.Char qualified as C (isDigit, isSpace)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

data Hand = Hand {cards :: [Card], handType :: Int}
  deriving (Show)

newtype Card = Card {value :: Char}
  deriving (Eq, Show)

instance Ord Card where
  compare c1 c2 = compare (r $ value c1) (r $ value c2)
    where
      r 'T' = 10
      r 'J' = 11
      r 'Q' = 12
      r 'K' = 13
      r 'A' = 14
      r c = read [c] :: Int

instance Eq Hand where
  (==) h1 h2 = cards h1 == cards h2

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1 h2 = case compare (handType h1) (handType h2) of
    EQ -> e' (cards h1) (cards h2)
      where
        e' [] [] = EQ
        e' (x1 : xs1) (x2 : xs2) =
          let r = compare x1 x2
           in if r == EQ then e' xs1 xs2 else r
    x -> x

parseHand :: String -> Hand
parseHand s = Hand (map Card s) (t $ p s)
  where
    p :: String -> [Int]
    p s = L.sort $ M.foldr (:) [] (L.foldr (\v m -> M.insert v (fromMaybe 0 (M.lookup v m) + 1) m) M.empty s)
    t [1, 1, 1, 1, 1] = 1
    t [1, 1, 1, 2] = 2
    t [1, 2, 2] = 3
    t [1, 1, 3] = 4
    t [2, 3] = 5
    t [1, 4] = 6
    t [5] = 7

data Result = Result {hand :: Hand, bid :: Int}
  deriving (Show, Eq)

instance Ord Result where
  compare r1 r2 = compare (hand r1) (hand r2)

parseLine :: String -> Result
parseLine s = Result (parseHand h) (read b)
  where
    h = takeWhile (not . C.isSpace) s
    b = drop (L.length h + 1) s

main :: IO ()
main = do
  content <- readFile "input/07.txt"
  -- content <- readFile "input/demo_07.txt"
  print $ s 1 $ L.map bid $ L.sort $ map parseLine $ lines content
  where
    s :: Int -> [Int] -> Int
    s _ [] = 0
    s i (x : xs) = i * x + s (i + 1) xs
