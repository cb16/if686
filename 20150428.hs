--- foldr ((+).(.).map) ---

map => (x -> y) -> [x] -> [y]
inputMap = (x -> y)
outputMap = [x] -> [y]

(.) => (b -> c) -> (a -> b) -> a -> c
inputComp = (b -> c)
OutputComp = (a -> b) -> a -> c

(+) => Num z => z -> z -> z
inputSoma = z
OutputSoma = z -> z

outputMap = inputComp
[x] -> [y] = (b -> c)
b = [x]
c = [y]

inputMap -> OutputComp
(x -> y) -> (a -> b) -> a -> c
Substituindo:
(.).map :: (x -> y) -> (a -> [x]) -> a -> [y]

agora:
InputCompMap = (x -> y)
OutputCompMap = (a -> [x]) -> a -> [y]

então, temos que:
OutputCompMap = inputSoma
(a -> [x]) -> a -> [y] = z

InputCompMap -> OutputSoma
(x -> y) -> z -> z
mas podemos substituir z
(x -> y) -> (a -> [x]) -> a -> [y] -> (a -> [x]) -> a -> [y]

agora, vamos para o foldr
foldr :: (t -> r -> r) -> r -> [t] -> r
t = (x -> y)
r = ((a -> [x]) -> a -> [y])

então, vamos substituir:
foldr ((+).(.).map) :: Num ((a -> [x]) -> a -> [y]) => ((a -> [x]) -> a -> [y]) -> [(x -> y)] -> ((a -> [x]) -> a -> [y])

--- (\x y z -> foldr z x y).map ---

foldr :: (t -> r -> r) -> r -> [t] -> r
(\x y z -> foldr z x y)
x = r
y = [t]
z = (t -> r -> r)
foldr :: r -> [t] -> (t -> r -> r) -> r
InputFoldr = r
OutputFoldr = [t] -> (t -> r -> r) -> r

map :: (x -> y) -> [x] -> [y]
inputMap = (x -> y)
outputMap = [x] -> [y]

OutputMap = InputFoldr
([x] -> [y]) = r

inputMap -> OutputFoldr
(x -> y) -> [t] -> (t -> ([x] -> [y]) -> ([x] -> [y])) -> ([x] -> [y])

--- map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) ---

map.((.) (foldr (++) (foldr (++) [] [[1], [2]]) ) )

foldr (++) [] [[1], [2]] :: Num f => [f]

foldr (++) (foldr (++) [] [[1], [2]]) :: Num f => [[f]] -> [f]

(.) :: (b -> c) -> (a -> b) -> a -> c
e temos que:
(.) (foldr (++) (foldr (++) [] [[1], [2]])) 
em que (b -> c) = [[f]] -> [f]
pois é a primeira função aplicada na composição.
logo:
b = [[f]]
c = [f]
InputCompFold = (a -> [[f]])
OutputCompFold = a -> [f]

map :: (x -> y) -> [x] -> [y]
inputMap = (x -> y)
outputMap = [x] -> [y]

OutputCompFold = InputMap
a -> [f] = (x -> y)
x = a
y = [f]

InputCompFold -> Output
(a -> [[f]]) -> [x] -> [y]
substituindo:
(a -> [[f]]) -> [a] -> [[f]]
concluindo:
map.((.) (foldr (++) (foldr (++) [] [[1], [2]]) ) ) :: Num f => (a -> [[f]]) -> [a] -> [[f]]