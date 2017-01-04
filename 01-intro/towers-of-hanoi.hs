type Peg = String
type Move = (Peg, Peg)

-- Get a list of steps to solve the [Tower Of Hanoi](https://en.wikipedia.org/wiki/Tower_of_Hanoi) puzzle
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to temp
  | n <= 0 = []
  | otherwise = (hanoi (n-1) from temp to) ++ [(from, to)] ++ (hanoi (n-1) temp to from)

-- Like `hamoi`, but with four pegs instead
hochiminh :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hochiminh n from to temp1 temp2
  | n <= 0 = []
  | otherwise = (hochiminh (n-2) from temp1 temp2 to) ++ [(from, temp2), (from, to), (temp2, to)] ++ (hochiminh (n-2) temp1 to from temp2)
