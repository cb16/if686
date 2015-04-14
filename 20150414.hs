-- TRABALHO 07

--Questão 1:

comp :: (u->z) -> (t->u) -> t -> z
comp f1 f2 num = f1 (f2 num)

compose :: (u -> z) -> [(t -> u)] -> [(t->z)]
compose f list = [comp f a | a <- list]

--Questão 2:

data Graph t = NilT | Node t [(Graph t)] deriving (Eq, Show)
gr = (Node 1 [(Node 3 [(Node 5 [(NilT)]), (Node 6 [(NilT)])]), (Node 2 [(Node 4 [(NilT)]), (Node 3 [(Node 5 [(NilT)]), (Node 6 [(NilT)])])]), (Node 7 [(NilT)])])

mapGraph :: (Graph t) -> (t -> t) -> (Graph t)
mapGraph (NilT) _ = (NilT)
mapGraph (Node at list) function = (Node (function at) [mapGraph a function | a<-list])

doFold :: (t->u->u) -> u -> [t] -> u
doFold function value [] = value
doFold function value (a:as) = function a (doFold function value as)

getVertices :: (Eq t) => [(Graph t)] -> [t] -> (Bool, [t])
getVertices [] visited = (False, visited)
getVertices [(NilT)] visited = (False, visited)
getVertices ((Node at list):as) visited = (getVertices ([ (Node val lt) | (Node val lt) <- list, ([ a | a<-visited, val==a] == [])] ++ as) (at:visited))

foldGraph :: (Eq t) => (t->u->u) -> u -> (Graph t) -> u
foldGraph function value graph = doFold function value (snd(getVertices [graph] []))

--Questão 3:

data Tree t = Emp | N t (Tree t) (Tree t) deriving (Eq, Show)
tr = (N 5 (N 7 (N 15 Emp (N 6 Emp Emp)) (N 2 Emp Emp)) (N 10 Emp Emp))

filtering :: (t->Bool) -> Tree t -> (Tree t, [Tree t])
filtering function Emp = (Emp, [])
filtering function tree@(N at left right) | function at = ((N at t1 t2), (r1++r2))
										  | otherwise = (Emp, [(left), (right)])
											  	where (t1, r1) = (filtering function left);
										  			  (t2, r2) = (filtering function right)

filterTree :: (t->Bool) -> (Tree t) -> [(Tree t)]
filterTree function Emp = []
filterTree function tree@(N at left right) | function at = [(cleanTree)] ++ concat (map (filterTree function) rest)
										   | otherwise = (filterTree function left) ++ (filterTree function right)
										   		where (cleanTree, rest) = (filtering function tree)