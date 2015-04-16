--TRABALHO 8:

quicksort :: (Ord t, Num t) => [t] -> [t]
quicksort [] = []
quicksort (a:as) = quicksort ([b | b<-as, b <= a]) ++ [a] ++ quicksort ([b | b<-as, b>a])

method :: (Ord t, Num t) => [t] -> [t] -> [[t]]
method l1 [] = [l1]
method l1 (b:bs) = [[x|x<-l1, x<=b]] ++ (method [x|x<-l1, x>b] bs)

divide :: (Ord t, Num t) => [t] -> [t] -> [[t]]
divide l1 l2 = [a | a<-(method l2 l1), a/=[]]

listPartitioner :: (Ord t, Num t) => [t] -> ([t]->[[t]])
listPartitioner list = \x -> (divide (quicksort list) (quicksort x))