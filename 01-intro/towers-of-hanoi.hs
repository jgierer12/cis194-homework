type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to temp
  | n <= 0 = []
  | otherwise = (hanoi (n-1) from temp to) ++ [(from, to)] ++ (hanoi (n-1) temp to from)
