--TRABALHO entregue no dia 24

--mergesort

mergesort :: [Int] -> [Int] -- método que usa Take e Drop - não sei se pode usar :T
mergesort list | list == [] = []
			   | (tail list) == [] = list
			   | otherwise = gather (mergesort (take (div (length list) 2) list)) (mergesort (drop (div (length list) 2) list))
			   -- take e drop são em O(n)

merge :: [Int] -> [Int]
merge list | list == [] = []
		   | (tail list) == [] = list
		   | otherwise = gather (merge (firstHalf list 0 ((div (length list) 2)-1))) (merge (secondHalf list 0 ((div (length list) 2)-1)))

firstHalf :: [Int] -> Int -> Int -> [Int] -- O(n)
firstHalf list at numero | list ==[]=[]
						 | at == numero = [head list]
						 | otherwise = (head list) : (firstHalf (tail list) (at+1) numero)

secondHalf :: [Int] -> Int -> Int -> [Int] -- O(n)
secondHalf list at numero | list == [] = []
						  | at == numero = tail list
						  | otherwise = (secondHalf (tail list) (at+1) numero)

gather :: [Int] -> [Int] -> [Int]
gather ar1 ar2 | ar1 == [] = ar2
			   | ar2 == [] = ar1
			   | (head ar1) <= (head ar2) = (head ar1):(gather (tail ar1) ar2)
			   | otherwise = (head ar2):(gather ar1 (tail ar2))

--heapsort

swap :: Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] --O(n)
swap x1 x2 p1 p2 i lista
 | lista == [] = []
 | p1 == i = x2 : swap x1 x2 p1 p2 (i+1) (tail lista)
 | p2 == i = x1 : swap x1 x2 p1 p2 (i+1) (tail lista)
 | otherwise = (head lista) : (swap x1 x2 p1 p2 (i+1) (tail lista))

getValue :: [Int] -> Int -> Int -> Int --O(n)
getValue lista p1 i
 | p1 == i = (head lista)
 | otherwise = getValue (tail lista) p1 (i+1)

heapfy :: [Int] -> Int -> Int -> [Int]
heapfy lista node leng
 | lista == [] = []
 | (2*node+1) >= leng  = lista
 | (2*node+2) < leng && (getValue lista node 0) <= getValue (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) (2*node+1) 0 && (getValue lista node 0) <= (getValue (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) (2*node+2) 0) = (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng)
 | (2*node + 2) < leng && (getValue lista node 0) > getValue (heapfy (heapfy lista (2*node + 1) leng) (2*node + 2) leng) (2*node + 1) 0	= heapfy (heapfy (swap (getValue lista node 0) (getValue (heapfy (heapfy lista (2*node + 1) leng) (2*node + 2) leng) (2*node + 1) 0) node (2*node + 1) 0 (heapfy (heapfy lista (2*node + 1) leng) (2*node + 2) leng)) (2*node + 1) leng) node leng
 | (2*node+2) < leng && (getValue lista node 0) > getValue (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) (2*node+2) 0 = heapfy (heapfy (swap (getValue lista node 0) (getValue (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng) (2*node+2) 0) node (2*node+2) 0 (heapfy (heapfy lista (2*node+1) leng) (2*node+2) leng)) (2*node+2) leng) node leng
 | (getValue lista node 0) > getValue (heapfy lista (2*node+1) leng) (2*node+1) 0 = heapfy (heapfy (swap (getValue lista node 0) (getValue (heapfy lista (2*node+1) leng) (2*node+1) 0) node (2*node+1) 0 (heapfy lista (2*node+1) leng)) (2*node+1) leng) node leng
 | otherwise = lista

hsort :: [Int] -> [Int] --em média roda em O(n log n)
hsort lista
 | lista == [] = []
 | tail lista == [] = [head lista]
 | otherwise =  (++) [head (heapfy lista 0 (length lista))] (hsort (remove ((heapfy (swap (head (heapfy lista 0 (length lista))) (getValue (heapfy lista 0 (length lista)) ((length lista)-1) 0) 0 ((length lista)-1) 0 (heapfy lista 0 (length lista))) 0 ((length lista)-1)))))

remove :: [Int] -> [Int] --O(n)
remove lista
 | tail lista == [] = []
 | otherwise = (head lista):(remove (tail lista))

--Códigos de questões:

-- primeiros N pares de Fib
func :: Int -> Int -> Int -> [Int]
func n counter number | (modulo2 (fib number)) == 0 && counter == n - 1 = [fib number]
			  		  | (modulo2 (fib number)) == 0 && counter < n = (fib number):(func n (counter+1) (number+1))
			   		  | otherwise = (func n counter (number+1))

fib :: Int -> Int
fib n | n == 0 || n==1 = 1
	  | otherwise = (fib (n-1)) + (fib (n-2))

modulo2 :: Int -> Int
modulo2 n = mod n 2

firstEvenFib :: Int -> [Int]
firstEvenFib number = (func number 0 0)

-- retorna a lista ordenada em funcao da soma de seus digitos
convertToString :: [Int] -> [String]
convertToString [] = []
convertToString (a:t) = (show a):(convertToString t)

convertToInt :: [String] -> [Int]
convertToInt [] = []
convertToInt (a:t) = (read a :: Int):(convertToInt t)

deleteMember :: [String] -> String -> [String]
deleteMember list toDelete | list == [] = []
						   | head list == toDelete = tail list
						   | otherwise = (head list):(deleteMember (tail list) toDelete)

getBig :: [String] -> String
getBig list | list == [] = []

ord :: [String] -> [String]
ord list | list == [] = []
		 | otherwise = (getBig list):(ord tail list)

ordenar :: [Int] -> [Int]
ordenar list = (convertToInt (ord (convertoToString list)))

--QUESTÕES DA AULA DO DIA 24:

--menorMaior
maior :: Int -> Int -> Int
maior x y | x > y = x
		  | otherwise = y

menor :: Int -> Int -> Int
menor x y | x < y = x
		  | otherwise = y

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = ((menor a (menor b c)),(maior a (maior b c)))

--OrdenaTripla
meio :: Int -> Int -> Int -> Int
meio a b c | (maior a (maior b c)) == a && (menor a (menor b c)) == b = c
		   | (maior a (maior b c)) == b && (menor a (menor b c)) == a = c
		   | (maior a (maior b c)) == a && (menor a (menor b c)) == c = b
		   | (maior a (maior b c)) == c && (menor a (menor b c)) == a = b
		   | (maior a (maior b c)) == b && (menor a (menor b c)) == c = a
		   | otherwise = a

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a,b,c) = ((menor a (menor b c)), (meio a b c) ,(maior a (maior b c)))

--exercícios com reta
type Ponto  = (Float, Float)
type Reta = (Ponto, Ponto)

cordPonto1 :: Ponto -> Float
cordPonto1 (x,y) = x

cordPonto2 :: Ponto -> Float
cordPonto2 (x,y) = y

vertical :: Reta -> Bool
vertical ((x1,y1), (x2,y2)) = x1==x2

pontoY :: Float -> Reta -> Float
pontoY a ((x1,y1), (x2,y2)) | x1==x2 = 0
							| otherwise = (d*e)/b - y1
							  where 
							  d = y2-y1
							  e = a-x1
							  b = x2-x1

--compreensão:

membro :: [Int] -> Int -> Bool
membro list num  = ([True| at <- list, at == num]) /= []

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados

baseExemplo = [("Sergio","O Senhor dos Aneis"),("Andre","Duna"),("Fernando","Jonathan Strange & Mr. Norrell"), ("Fernando","A Game of Thrones")]

livros :: BancoDados -> Pessoa -> [Livro]
livros banco pe = [snd x | x <- banco, (fst x == pe)]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos banco liv = [fst x | x <- banco, (snd x == liv)]

emprestado :: BancoDados -> Livro -> Bool
emprestado banco liv = [True| x <- banco, snd x == liv] /= []

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos banco pe = length [x | x <- banco, fst x == pe]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver banco pe liv = [x | x <- banco, ((fst x /= pe) && (snd x /= liv))]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort list = quicksort([x | x<-(tail list), x<=(head list)]) ++ ([head list] ++ quicksort([x | x<-(tail list), x>(head list)]))
