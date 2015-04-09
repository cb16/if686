data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi*(r*r);
area (Rectangle l c) = l*c;

data Dias = Domingo | Segunda Int [String] | Terca Int [String] | Quarta Int [String] | Quinta Int [String] | Sexta Int [String] | Sabado

--(i)
isFds :: Dias -> Bool
isFds (Domingo) = True
isFds (Sabado) = True
isFds (_) = False

--(ii)
func :: [String] -> Bool
func list = (length [a | a<-list, a=="PLC"]) /= 0;

hasPlc :: Dias -> Bool
hasPlc (Domingo) = False
hasPlc (Sabado) = False
hasPlc (Segunda num list) = func list;
hasPlc (Terca num list) = func list;
hasPlc (Quarta num list) = func list;
hasPlc (Quinta num list) = func list;
hasPlc (Sexta num list) = func list;

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq,Show)
--(Node 1 (Node 2 (Node 3 (Node 4 (NilT) (NilT)) (NilT)) (NilT)) (Node 5 (NilT) (NilT)))
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr deriving (Show)

showExpr :: Expr -> String --showExpr (Add (Lit 1) (Sub (Lit 3) (Lit 2)))
showExpr (Lit n) = show n
showExpr (Add ex1 ex2) = (showExpr ex1) ++ " + " ++ (showExpr ex2)
showExpr (Sub ex1 ex2) = (showExpr ex1) ++ " - " ++ (showExpr ex2)

data List t = Nil | Cons t (List t) deriving (Eq, Show)

toList :: List t -> [t] -- toList (Cons 1 (Cons 3 (Cons 5 (Nil))))
toList Nil = []
toList (Cons a as) = a:(toList as)

fromList :: [t] -> List t -- fromList [1,2,3]
fromList [] = Nil
fromList (a:as) = (Cons a (fromList as))

depth :: Tree t -> Int
depth NilT = -1
depth (Node a t1 t2) | (1 + (depth t1)) > (1+(depth t2)) = 1 + (depth t1)
					 | otherwise = 1 + (depth t2)

collapse :: Tree t -> [t]
collapse (NilT) = []
collapse (Node a t1 t2) = (collapse t1) ++ [a] ++ (collapse t2)

--bfs :: Tree t -> t -> Bool

