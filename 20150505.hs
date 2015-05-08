import Data.Char;

--Questão 1--

type Table = [(Int, Int)]

ex :: Table
ex = [(1,2), (2,4), (3,9)]

put :: Table -> (Int, Int) -> Maybe Table
put table (a, b)  = case hasKey table a of
						Nothing -> Just (table ++ [(a,b)])
						Just tupla -> Just table

remove :: Table -> Int -> Maybe Table
remove table value = case hasKey table value of
						Nothing -> Just table
						Just tupla -> Just ([x | x <- table, fst(x) /= value])

hasKey :: Table -> Int -> Maybe (Int, Int)
hasKey [] key = Nothing
hasKey ((a,b):as) key | a == key = Just (a,b)
					  | otherwise = hasKey as key

main1 :: Table -> Maybe Table
main1 table = do {
	v1 <- put table (1,2);
	v2 <- put v1 (2,3);
	v3 <- remove v2 1;
	v4 <- put v3 (3,4);
	v5 <- put v4 (4,5);
	v6 <- remove v5 2;
	v7 <- put v6 (5,6);
	v8 <- put v7 (6,7);
	v9 <- remove v8 3;
	v10 <- put v9 (7,8);
	v11 <- put v10 (8,9);
	v12 <- remove v11 4;
	v13 <- put v12 (9,10);
	v14 <- put v13 (10,11);
	v15 <- remove v14 5;
	return v15; -- resultado esperado: Just [(6,7), (7,8), (8,9), (9,10), (10,11)]
}

-- Questão 2 --

--getLine :: IO String

--putStr :: String -> IO ()

--getChar :: IO Char

notLetter :: Char -> Bool
notLetter c = (c>='a' && c<='z') || (c>='A' && c<='Z') || c==' '

allLetter :: String -> Bool
allLetter [] = True
allLetter (a:as) = (notLetter a) && (allLetter as)

test :: String -> Maybe String
test [] = Nothing
test (a:as) | allLetter (a:as) = Just (a:as)
		    | otherwise = Nothing

maiuscula :: Maybe String -> Maybe String
maiuscula Nothing = Nothing
maiuscula (Just word@(a:as)) = Just [toUpper c | c <- word]

help :: String -> String
help [] = []
help (a:as) | a==' ' = ('\n'):(help as)
			| otherwise = a:(help as)

try :: Maybe String -> String
try Nothing = ""
try (Just word) = help word

main :: IO ()
main = do {
	str <- getLine;
	toPrint <- return (test str);
	word <- return (maiuscula toPrint);
	list <- return (try word);
	putStrLn list;
}