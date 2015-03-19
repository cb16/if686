vendas :: Int -> Int
vendas n = 2

equal :: Int -> Int -> Int
equal s n | (vendas n) == s = 1
		  | otherwise = 0

func :: Int -> Int -> Int
func s n | n == 0 =  (equal s n)
		 | otherwise = (func s (n-1)) + (equal s n)