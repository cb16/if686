member :: [Int] -> Int -> Bool
member list num | list == [] = False
				| otherwise = (||) (member (tail list) num) (head list == num)

--outra forma
mem [] = False
mem (a:as) n = (a==n) || (mem as n)
				
double :: [Int] -> [Int]
double list | list == [] = []
			| otherwise = (++) [((head list) * 2)] (double (tail list))

--outro modo de fazer double - casamento de padrões			
double2 [] = []
double2 (a:as) = (a*2):(double2 as)
			
digits :: String -> String
digits word | word == [] = []
			| (head word >= '0') && (head word <= '9') = (++) [(head word)] (digits (tail word))
			| otherwise = digits (tail word)
			
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs list1 list2 | ((&&) (list1 == []) (list2 == [])) = []
					 | list1 == [] = list2
					 | list2 == [] = list1
					 | otherwise = (++) [(head list1) + (head list2)] (sumPairs (tail list1) (tail list2))

--outro modo
sumPairs2 [] [] = []
sumPairs2 [] (a:as) = (a:as)
sumPairs2 (a:as) [] = (a:as)
sumPairs2 (a:as) (b:bs) = (a+b):(sumPairs2 as bs)

--casamento de padroes
sumList [] = 0
sumList (a:as) = a + sumList as

--Quicksort
greater :: [Int] -> Int -> [Int]
greater list num | list == [] = []
				 | (head list > num) = (head list):(greater (tail list) num)
				 | otherwise = greater (tail list) num

less :: [Int] -> Int -> [Int]
less list num | list == [] = []
			  | (head list <= num) = (head list):(less (tail list) num)
			  | otherwise = less (tail list) num

quicksort :: [Int] -> [Int]
quicksort list | list == [] = []
			   | (tail list) == [] = [head list]
			   | otherwise = (++) (quicksort (less (tail list) (head list))) ((head list):(quicksort (greater (tail list) (head list))))
