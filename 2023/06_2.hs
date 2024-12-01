import Data.Char (isDigit, isSpace)

parseInt :: String -> Int
parseInt s = read $ p s
  where
    p [] = []
    p (x : xs) = if isDigit x then x : p xs else p xs

parseInput :: [String] -> [(Int, Int)]
parseInput (time : distance : _) = [(parseInt $ drop 5 time, parseInt $ drop 9 distance)]

calcBounds :: (Int, Int) -> (Int, Int)
calcBounds (t, d) = (l, h)
  where
    b = fromIntegral t / 2
    x = sqrt $ fromIntegral (t * t) / 4 - fromIntegral d
    l' = b - x
    h' = b + x
    l = if fromIntegral c - l' < 0.001 then c + 1 else c
      where
        c = ceiling l'
    h = if h' - fromIntegral c < 0.001 then c - 1 else c
      where
        c = floor h'

calcNum :: (Int, Int) -> Int
calcNum (l, h) = h - l + 1

main :: IO ()
main = do
  content <- readFile "input/06.txt"
  -- content <- readFile "input/demo_06.txt"
  print $ product $ map (calcNum . calcBounds) $ parseInput $ lines content
