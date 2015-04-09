--Trabalho 6:

data Arv t = Vazio | No t [(Int, Arv t)] deriving (Eq, Show)

grafo = (No 1 [(1, (No 3 [(1, (No 5 [(0, Vazio)])), (1, (No 6 [(0, Vazio)]))])), (1, (No 2 [(1, (No 4 [(0, Vazio)])), (1, (No 3 [(1, (No 5 [(0, Vazio)])), (1, (No 6 [(0, Vazio)]))]))])), (1, (No 7 [(0, Vazio)]))])

dfs :: (Eq t) => Arv t -> t -> Bool 
dfs (No a list) n = theDfs n [((No a list))] [a]

theDfs :: (Eq t) => t -> [Arv t] -> [t] -> Bool 
theDfs n [] _ = False 
theDfs n ((No topo subs):stail) visited | topo == n = True 
										| otherwise = (theDfs n ([ (No val lt) | (number, (No val lt)) <- subs, not (wasVisited val visited)] ++ stail) (topo:visited))

wasVisited :: (Eq t) => t -> [t] -> Bool 
wasVisited n visited = [ a | a<-visited, n==a] /= []
