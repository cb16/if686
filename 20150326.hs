-- TRABALHO 3

--questão 1

type Table = [(Int, Int)]

ex :: Table
ex = [(1,2), (2,4), (3,9)]

-- O(n)
get :: Table -> Int -> (Int, Int)
get [] key = (key, 0)
get tabela key | hasKey tabela key == False = (key, 0)
			   | otherwise = head ([x | x <- tabela, (fst x == key)])

-- O(n)
put :: Table -> (Int,Int) -> Table 
put table pair | (fst pair) < fst (head table) = [pair] ++ table
			   | (fst pair) == fst (head table) = (head table):(put (tail table) ((fst pair)+1, snd pair))
			   | otherwise = (head table):(put (tail table) pair)

-- O(n)
remove :: Table -> Int -> Table
remove [] element = []
remove table element = [x | x <- table, (snd x /= element)]

-- O(n)
hasKey :: Table -> Int -> Bool
hasKey tabela key = length ([x | x <- tabela, (fst x == key)]) == 1

-- questão 2
comparaConjuntos :: [Int] -> [Int] -> String
comparaConjuntos a b
 | a == [] && b == [] = "Conjuntos vazios"
 | a == [] = "B contem A"
 | b == []  = "A contem B" 
 | (teste a b) == [] = "Conjuntos disjuntos"
 | length (teste a b) == length (organizar (qs b)) && length (organizar (qs a)) == length (organizar (qs b)) = "Conjuntos iguais"
 | length (teste a b) == length (organizar (qs b)) && length (organizar (qs a)) > length (organizar (qs b)) = "A contem B"
 | length (teste a b) == length (organizar (qs a)) && length (organizar (qs a)) < length (organizar (qs b)) = "B contem A"
 | length (teste a b) < length (organizar (qs a)) && length (teste a b) < length (organizar (qs b)) = "A interseciona B"
 
teste :: [Int] -> [Int] -> [Int] 
teste a b = subconj (organizar (qs a)) (organizar (qs b)) 

subconj :: [Int] -> [Int] -> [Int]
subconj a b = [x| x <- a, (member b x)]

organizar :: [Int] -> [Int]
organizar (a:as)
 | as == [] = [a]
 | a /= (head as) = (++) [a] (organizar as)   
 | otherwise = organizar as

qs :: [Int] -> [Int] 
qs [] = []
qs (a:as) = (qs [x|x<-as, x<=a]) ++ (a:qs[y|y<-as, y > a])

member :: [Int] -> Int -> Bool
member lista a  = [x|x <- lista, (x==a)] /= []