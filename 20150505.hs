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

--------------

{-looking :: String -> Bool
looking [] = True
looking (a:as) | (a>='a' && a <= 'z' ) || (a>='A' && a <= 'Z') || a==' ' = looking as
			   | otherwise = False

check :: Maybe String -> Maybe String
check [] = Nothing
check (a:as) | looking (a:as) = Just (a:as)
			 | otherwise = Nothing

main2 :: IO ()
main2 = do {
	entrada <- getLine;
	validado <- check (Just entrada);
	--putStrLn validado;
	--return validado;
	putStr validado;
-}
