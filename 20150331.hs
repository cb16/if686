-- questão 1:

-- Em Haskell é possível criar funções com argumentos de um tipo mais geral, como por exemplo uma função soma :: t -> t -> t,
-- que, embora trabalhe apenas com números, pode receber um Char e já declarar um erro, podendo-se resolver isso com uma restrição
-- da função (soma :: (Num t) => t -> t -> t). Já com Java, os tipos dos argumentos são predefinidos, então, caso sejam colocados 
-- tipos distintos dos declarados, o erro ocorrera imediatamente, não sendo tolerada a generalidade oferecida por Haskell. 
-- Uma desvantagem dessa generalização de Haskell seria ter argumentos livres e não necessariamente identificar o que cada função 
-- de fato pode ter como parâmetro para realizar seu procedimento, causando inúmeros erros. Em Java, isso fica mais claro.



-- questão 2:
next :: String -> String
next number  | number == [] = ""
			 | otherwise = show (length (takeWhile (==(head number)) number)) ++ ((head number):(next (drop (length (takeWhile (==(head number)) number)) number)))

check :: Int -> Int -> String -> String
check n cont word | n==cont = word
				  | otherwise = check n (cont+1) (next word)

lookSay :: Int -> String
lookSay 0 = " "
lookSay n = check n 1 "1" --ou "d" ou "1", dependendo de como se deseja a sequência





--questão 3:

type No t = t
type Aresta t = (No t, No t)
type Grafo t = ([No t], [Aresta t])
type Grafo2 t = [(t, [t])]

grafo :: Grafo Int
grafo = ([1,2,3,4,5,6,7], [(1,2), (1,3), (1,4), (1,6), (4,5), (4,6), (5,6), (2,1), (3,1), (4,1), (6,1), (5,4), (6,4), (6,5)])
--[(3,[1]), (1,[3,2,4,6]), (2,[1]), (4,[1,5,6]), (6,[1,4,5]), (5,[4,6]), (7,[])]   >   grafo arrumado

search :: Eq t => Grafo t -> t -> t -> [Aresta t] --inicia com um grafo de (nos, arestas)
search graf ini fim = (aresta graf ini fim)       -- se ini==fim, então não há arestas a percorrer, resposta=[]

aresta :: Eq t => Grafo t -> t -> t -> [Aresta t] --de um grafo de (nos, arestas), temos um de [(no, [listaAdj])]
aresta graf ini fim = aresta2 (search2 (arrumarGrafo graf) ini fim) (snd graf)

aresta2 :: Eq t => [t] -> [Aresta t] -> [Aresta t] --contrucao de lista de arestas a partir dos vertices visitados
aresta2 [] _ = []
aresta2 lista arestas 
 | (tail lista) == [] = []
 | otherwise = [x|x <- arestas, (head lista == fst x) && ((head (tail lista)) == snd x)]++(aresta2 (tail lista) arestas)

ver :: Eq t => [Aresta t] -> t -> t -> [(t, t)] --buscar aresta correspondente a dois vertices
ver arestas f s = [x|x <- arestas, (fst x==f) && (snd x==s)]

search2 :: Eq t => Grafo2 t -> t -> t -> [t] --faz a busca pelo caminho em si, retornando os vertices visitados
search2 graf ini fim
 | ini == fim = [ini]
 | (not (member graf ini)) || (not (member graf fim)) = []
 | newGraf == [] = []
 | otherwise = ini:(head newGraf)
 	where
 		procurar = [search2 (tirarNo graf ini) x fim | x <- (lista graf ini), (member graf x)]
 		newGraf = [x|x <- procurar, x /= []]
 		
tirarNo :: Eq t => Grafo2 t -> t -> Grafo2 t --tira vertice ja visitado
tirarNo graf v = [x|x <- graf, (fst x) /= v]

arrumarGrafo :: Eq t => Grafo t -> Grafo2 t --de (nos, arestas) para [(no, [listaAdj])]
arrumarGrafo graf
 | (fst graf) == [] = []
 | otherwise = val:(arrumarGrafo ((tail (fst graf)), (snd graf))) 
 	where val = ((head (fst graf)), (adjacentes (snd graf) (head (fst graf))))

adjacentes :: Eq t => [Aresta t] -> t -> [t] --pegar adjacentes em Grafo para ajustar para Grafo2
adjacentes [] x = []
adjacentes ((v1,v2):as) x
 | (v1 == x) = v2:(adjacentes as x)
 | (v2 == x) = v1:(adjacentes as x)
 | otherwise = adjacentes as x

lista:: Eq t => Grafo2 t -> t -> [t] --retira apenas a lista de adjacencia de Grafo2
lista [] v = []
lista graf v
	| (fst (head graf) == v) = snd (head graf)
	| otherwise = lista (tail graf) v

member :: Eq t => Grafo2 t -> t -> Bool --verifica se um vertice existe no grafo
member graf v = [x|x <- graf, (fst x) == v] /= []



--questão 4:
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort list = (quicksort [x | x<-(tail list), x <= (head list)]) ++ ((head list):(quicksort [x | x<-(tail list), x>(head list)]))

getMediana :: [Int] -> Int
getMediana list = head (drop (div (length list) 2) (quicksort list))

--filtroMediana [[Int]] -> Int -> [[Int]]

