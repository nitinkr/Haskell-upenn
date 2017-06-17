hanoi :: Integer -> String -> String -> String -> [(String, String)]
hanoi 0 a b c = [(a, b)]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

hanoi4 :: Integer -> String -> String -> String -> String -> [(String, String)]
hanoi4 0 a b c d = []
hanoi4 1 a b c d = [(a, d)]
hanoi4 n a b c d = (hanoi4 (div n 2) a d c b) ++  (hanoi (n  - (div n 2)) a c d) ++ (hanoi4 (div n 2) b a c d)
--hanoi4 n a b c d = (hanoi4 (n-1) a d c b) ++  [(a, d)] ++ (hanoi4 (n - 1) b a c d)

