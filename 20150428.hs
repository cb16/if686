-- a) foldr (+).(.).map == (foldr (+)).((.).map)

* (.).map

map :: (a -> b) -> [a] -> [b]
in1: (a -> b)
out1: [a] -> [b]

(.) :: (e -> f) -> (d -> e) -> d -> f
in2: (e -> f)
out2: (d -> e) -> d -> f


out1 = in2
[a] -> [b] = (e -> f)
[a] = e
[b] = f

in1 -> out2
(a -> b) -> (d -> e) -> d -> f
(a -> b) -> (d -> [a]) -> d -> [b]

(.).map :: (a -> b) -> (d -> [a]) -> d -> [b]

* foldr (+)

foldr :: (s -> z -> z) -> z -> [s] -> z
in1: (s -> z -> z)
out1: z -> [s] -> z

(+) :: Num x => x -> x -> x

in1 = (+)
(s -> z -> z) = (x -> x -> x)
s = x
z = x

foldr (+) -> x -> [x] -> x


* (foldr (+)).((.).map)

(.).map :: (a -> b) -> (d -> [a]) -> d -> [b]
in1: (a -> b)
out1: (d -> [a]) -> d -> [b]

foldr (+) -> x -> [x] -> x
in2: x
out2: [x] -> x


out1 = in2
((d -> [a]) -> d -> [b]) = x

in1 -> out2
(a -> b) -> [x] -> x
(a -> b) -> [((d -> [a]) -> d -> [b])] -> ((d -> [a]) -> d -> [b])


-- b) (\x y z -> foldr z x y).map

map :: (a -> b) -> [a] -> [b]
in1: (a -> b)
out1: [a] -> [b]

foldr :: (m -> n -> n) -> n -> [m] -> n
(\x y z) :: n -> [m] -> (m -> n -> n) -> n
in2: n
out2: [m] -> (m -> n -> n) -> n


in2 = out1
n = ([a] -> [b])

in1 -> out2
(a -> b) -> [m] -> (m -> n -> n) -> n
(a -> b) -> [m] -> (m -> ([a] -> [b]) -> [a] -> [b]) -> [a] -> [b]

(\x y z -> foldr z x y).map :: (a -> b) -> [m] -> (m -> ([a] -> [b]) -> [a] -> [b]) -> [a] -> [b]


-- c) map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))

foldr (++) [] [[1], [2]] :: Num a => [a]

foldr (++) (foldr (++) [] [[1], [2]]) :: Num a => [[a]] -> [a]

(.) :: (e -> f) -> (d -> e) -> d -> f
in: (e -> f)
out: (d -> e) -> d -> f

in = foldr (++) (foldr (++) [] [[1], [2]])
(e -> f) = [[a]] -> [a]
e = [[a]]
f = [a]

(.) (foldr (++) (foldr (++) [] [[1], [2]])) 
in1: (d -> e) = (d -> [[a]])
out1: (d -> f) = (d -> [a])

map :: (x -> y) -> [x] -> [y]
in2: (x -> y)
out2: [x] -> [y]

* map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))

out1 = in2
(d -> [a]) = (x -> y)
x = d
y = [a]

in1 -> out2
(d -> [[a]]) -> [d] -> [[a]]


map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: (Num a) => (d -> [[a]]) -> [d] -> [[a]]


-- d) (foldr).(.)$(!!)

* (foldr).(.)

(.) :: (e -> f) -> (d -> e) -> d -> f
in1: (e -> f)
out1: (d -> e) -> d -> f

foldr :: (s -> z -> z) -> z -> [s] -> z
in2: (s -> z -> z)
out2: z -> [s] -> z

out1 = in2
(d -> e) -> d -> f = (s -> z -> z)
s = (d -> e)
z = d
z = f
(d = f)

in1 -> out2
(e -> f) -> z -> [s] -> z
(e -> f) -> f -> [f -> e] -> f

(foldr).(.) :: (e -> f) -> f -> [f -> e] -> f

* (foldr).(.)$(!!)

(!!) :: [a] -> Int -> a

(foldr).(.) :: (e -> f) -> f -> [f -> e] -> f
in: (e -> f)
out: f -> [f -> e] -> f

in = (!!)
(e -> f) = [a] -> (Int -> a)
e = [a]
f = (Int -> a)


(foldr).(.)$(!!) :: (Int -> a) -> [(Int -> a) -> [a]] -> Int -> a