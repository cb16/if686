{-

A basic interpreter for a purely functional subset of Scheme named SkimScheme.
Part of this interpreter has been derived from the "Write Yourself a Scheme in
48 Hours - An Introduction to Haskell through Example", by Jonathan Tang. It
does not implement a number of Scheme's constructs. Moreover, it uses a
different approach to implement mutable state within the language.

The name "SkimScheme" refers to the stripped down nature of this interpreter.
According to the New Oxford American Dictionary, "skim" can mean:

(as a verb) ... read (something) quickly or cursorily so as to note only
the important points.

(as a noun) ... an act of reading something quickly or superficially. 

"skimmed/skim milk" is milk from which the cream has been removed. 

The name emphasizes that we do not want to cover the entire standard, small as
it may be. Instead, we want to focus on some of the important aspects, taking a
language implementer's point of view, with the goal of using it as a teaching
tool. Many, many, many aspects of Scheme standards are not covered (it does not
even support recursion!).

Written by Fernando Castor
Started at: August 28th 2012
Last update: December 17th 2012

-}

module Main where
import System.Environment
import Control.Monad
import Data.Map as Map
import Debug.Trace as Trace
import LispVal
import SSParser
import SSPrettyPrinter

-----------------------------------------------------------
--                      INTERPRETER                      --
-----------------------------------------------------------
eval :: StateT -> LispVal -> StateTransformer LispVal
eval env val@(String _) = return val
eval env val@(Atom var) = stateLookup env var 
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "begin":[v])) = eval env v
eval env (List (Atom "begin": l: ls)) = (eval env l) >>= (\v -> case v of { (error@(Error _)) -> return error; otherwise -> eval env (List (Atom "begin": ls))})
eval env (List (Atom "begin":[])) = return (List [])  
eval env iff@(List (Atom "if":condition:consequent:[])) = 
    eval env condition 
    >>= (\x -> case x of
      (Bool True) -> eval env consequent
      (Bool False) -> return (List [])
      erro@(Error msg) -> return erro
      otherwise -> return (Error "Not a boolean expression!"))
eval env iff@(List (Atom "if":condition:consequent:alternate:[])) = 
    eval env condition 
    >>= (\x -> case x of
      (Bool True) -> eval env consequent
      (Bool False) -> eval env alternate
      erro@(Error msg) -> return erro
      otherwise -> return (Error "Not a boolean expression!"))
eval env lam@(List (Atom "lambda":(List formals):body:[])) = return lam
eval env stuff@(List (Atom "set!":(Atom variable):(a):[])) = (stateLookup env variable) >>= (\v -> case v of { (error@(Error _)) -> return error; otherwise -> (define env ([Atom variable]++[a])) })
eval env letFunction@(List (Atom "let":(List definitions):body:[])) = lett env [letFunction]
-- The following line is slightly more complex because we are addressing the
-- case where define is redefined by the user (whatever is the user's reason 
-- for doing so. The problem is that redefining define does not have
-- the same semantics as redefining other functions, since define is not
-- stored as a regular function because of its return type.
eval env (List (Atom "define": args)) = maybe (define env args) (\v -> return v) (Map.lookup "define" env)
eval env (List (Atom func : args)) = mapM (eval env) args >>= apply env func 
eval env (Error s)  = return (Error s)
eval env form = return (Error ("Could not eval the special form: " ++ (show form)))

stateLookup :: StateT -> String -> StateTransformer LispVal
stateLookup env var = ST $ 
  (\s -> 
    (maybe (Error "variable does not exist.") 
           id (Map.lookup var (union s env) 
    ), s))


-- Because of monad complications, define is a separate function that is not
-- included in the state of the program. This saves  us from having to make
-- every predefined function return a StateTransformer, which would also
-- complicate state management. The same principle applies to set!. We are still
-- not talking about local definitions. That's a completely different
-- beast.
define :: StateT -> [LispVal] -> StateTransformer LispVal
define env [(Atom id), (List ([(Atom "make-closure"), (List (Atom "lambda":(List definitions):body:[]))]))] =   
    eval env (List (Atom "lambda":(List definitions):body:[])) >>= (\loofsloofs -> case loofsloofs of { erro@(Error _) -> return erro; otherwise -> defineVar env id (MakeClosure loofsloofs env)})
    --defineVar env id (MakeClosure (List (Atom "lambda":(List definitions):body:[])) env)
define env [(Atom id), val] = defineVar env id val
define env [(List [Atom id]), val] = defineVar env id val 
define env [List ((Atom id):definitions), body] = eval env (List (Atom "define":(Atom id):[(List (Atom "lambda":(List definitions):body:[]))]))
define env args = return (Error "wrong number of arguments")
defineVar env id (MakeClosure a b) =
  ST (\s -> let (ST f)    = eval b a
                (result, newState) = f s
            in (result, (insert id (MakeClosure result newState) newState))
     )
defineVar env id val = 
  ST (\s -> let (ST f)    = eval env val
                (result, newState) = f s
            in (result, (insert id result newState))
     )

lett :: StateT -> [LispVal] -> StateTransformer LispVal
lett env [(List (Atom "let": (List(definitions)): body:[]))] = do
    let val = (defineLet env definitions) >> (eval env body) >>= (\x -> deleteLet env definitions x)
    let (a,b) = getResult val;
    mergeState val (return a) env; -- em val tem quaisquer variaveis que forem definidas em let
                                   -- em a tem o valor calculado no let 

defineLet :: StateT -> [LispVal] -> StateTransformer LispVal
defineLet env [(List ([(Atom id), val]))] = defineVar env id val
defineLet env ((List ([(Atom id), val])):as) = (defineVar env id val) >> (defineLet env as)
defineLet env a = return (Error "Invalid parameters!")

deleteLet :: StateT -> [LispVal] -> LispVal -> StateTransformer LispVal
deleteLet env [(List ([(Atom id), val]))] st = deleteVar env id st
deleteLet env ((List ([(Atom id), val])):as) st = (deleteVar env id st) >> (deleteLet env as st)
deleteLet env a st = return (Error "Invalid parameters!")
deleteVar env id st =
  ST (\s -> let (ST f) = eval env st
                (result, newState) = f s
            in (result, (delete id newState))
     ) 

mergeState :: StateTransformer LispVal -> StateTransformer LispVal -> StateT -> StateTransformer LispVal
mergeState (ST f) (ST g) env = ST (\s -> let (v, newS) = f env -- v é o valor de val
                                             (v2, newS2) = g env -- v2 é o valor de a que é igual ao valor de val
                                          in  (v, union s (difference newS newS2))
                                  )

-- The maybe function yields a value of type b if the evaluation of 
-- its third argument yields Nothing. In case it yields Just x, maybe
-- applies its second argument f to x and yields (f x) as its result.
-- maybe :: b -> (a -> b) -> Maybe a -> b
apply :: StateT -> String -> [LispVal] -> StateTransformer LispVal
apply env func args =  
                  case (Map.lookup func env) of
                      Just (Native f)  -> return (f args)
                      otherwise -> 
                        (stateLookup env func >>= \res -> 
                          case res of 
                            (MakeClosure (List (Atom "lambda" : List formals : body:l)) b) -> do -- se eu só avaliasse, ele iria juntar o estado real atual com esse b, ai ia dar merda
                                let thisState@(ST f) = lambda b formals body args
                                let (val, state) = getResult thisState
                                let (r, alterB) = (f b)
                                ST ( \parameters -> (val, (insert func (MakeClosure (List (Atom "lambda" : List formals : body:l)) (union alterB environment)) parameters)) 
                                   )
                            List (Atom "lambda" : List formals : body:l) -> do
                                let lifslifs = (union (insert func res env) environment)
                                let estado@(ST f) = (lambda lifslifs formals body args)
                                let (a,b) = getResult estado
                                return a
                            --lambda env formals body args                              
                            otherwise -> return (Error ("not a function."++(show(func))))
                        )
 
-- The lambda function is an auxiliary function responsible for
-- applying user-defined functions, instead of native ones. We use a very stupid 
-- kind of dynamic variable (parameter) scoping that does not even support
-- recursion. This has to be fixed in the project.
lambda :: StateT -> [LispVal] -> LispVal -> [LispVal] -> StateTransformer LispVal
lambda env formals body args = do
  let dynEnv = Prelude.foldr (\(Atom f, a) m -> Map.insert f a m) env (zip formals args)
  ST $ (\s -> let (ST f) = eval dynEnv body
                  (result, newState) = f s
              in (result, (difference newState environment))
       )

-- Initial environment of the programs. Maps identifiers to values. 
-- Initially, maps function names to function values, but there's 
-- nothing stopping it from storing general values (e.g., well-known
-- constants, such as pi). The initial environment includes all the 
-- functions that are available for programmers.
environment :: Map String LispVal
environment =   
            insert "number?"        (Native predNumber)
          $ insert "boolean?"       (Native predBoolean)
          $ insert "list?"          (Native predList)
          $ insert "+"              (Native numericSum) 
          $ insert "*"              (Native numericMult) 
          $ insert "-"              (Native numericSub) 
          $ insert "car"            (Native car)           
          $ insert "cdr"            (Native cdr) 
          $ insert "=="             (Native equals)
          $ insert ">"              (Native greater)
          $ insert "<"              (Native less)
          $ insert ">="             (Native greaterOrEquals)
          $ insert "<="             (Native lessOrEquals)
          $ insert "It?"            (Native it)
          $ insert "/"              (Native division)
          $ insert "mod"            (Native modulo)
          $ insert "comment"        (Native comment)
          $ insert "cons"           (Native cons)
          $ insert "eqv?"           (Native eqv)
            empty

type StateT = Map String LispVal

-- StateTransformer is a data type that embodies computations
-- that transform the state of the interpreter (add new (String, LispVal)
-- pairs to the state variable). The ST constructor receives a function
-- because a StateTransformer gets the previous state of the interpreter 
-- and, based on that state, performs a computation that might yield a modified
-- state (a modification of the previous one). 
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
  return x = ST (\s -> (x, s))
  (>>=) (ST m) f = ST (\s -> let (v, newS) = m s
                                 (ST resF) = f v
                             in  resF newS
                      )
    
-----------------------------------------------------------
--          HARDWIRED PREDEFINED LISP FUNCTIONS          --
-----------------------------------------------------------

-- Includes some auxiliary functions. Does not include functions that modify
-- state. These functions, such as define and set!, must run within the
-- StateTransformer monad. 

car :: [LispVal] -> LispVal
car [List (a:as)] = a
car [DottedList (a:as) _] = a
car ls = Error "invalid list."

cdr :: [LispVal] -> LispVal
cdr (List (a:as) : ls) = List as
cdr (DottedList (a:[]) c : ls) = c
cdr (DottedList (a:as) c : ls) = DottedList as c
cdr ls = Error "invalid list."

predNumber :: [LispVal] -> LispVal
predNumber (Number _ : []) = Bool True
predNumber (a:[]) = Bool False
predNumber ls = Error "wrong number of arguments."

predBoolean :: [LispVal] -> LispVal
predBoolean (Bool _ : []) = Bool True
predBoolean (a:[]) = Bool False
predBoolean ls = Error "wrong number of arguments."

predList :: [LispVal] -> LispVal
predList (List _ : []) = Bool True
predList (a:[]) = Bool False
predList ls = Error "wrong number of arguments."

numericSum :: [LispVal] -> LispVal
numericSum [] = Number 0
numericSum l = numericBinOp (+) l

numericMult :: [LispVal] -> LispVal
numericMult [] = Number 1
numericMult l = numericBinOp (*) l

numericSub :: [LispVal] -> LispVal
numericSub [] = Error "wrong number of arguments."
-- The following case handles negative number literals.
numericSub [x] = if onlyNumbers [x]
                 then (\num -> (Number (- num))) (unpackNum x)
                 else Error "not a number."
numericSub l = numericBinOp (-) l

-- We have not implemented division. Also, notice that we have not 
-- addressed floating-point numbers.

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op args = if onlyNumbers args 
                       then Number $ foldl1 op $ Prelude.map unpackNum args 
                       else Error "not a number."
                       
onlyNumbers :: [LispVal] -> Bool
onlyNumbers [] = True
onlyNumbers (Number n:ns) = onlyNumbers ns
onlyNumbers ns = False             

onlyStrings :: [LispVal] -> Bool
onlyStrings [] = True
onlyStrings (String n:ns) = onlyStrings ns
onlyStrings ns = False

onlyBools :: [LispVal] -> Bool
onlyBools [] = True
onlyBools (String n:ns) = onlyBools ns
onlyBools ns = False
                       
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
--- unpackNum a = ... -- Should never happen!!!!

unpackString :: LispVal -> String
unpackString (String n) = n

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b

equals :: [LispVal] -> LispVal
equals list@(f:s:[]) | (onlyBools list) = (Bool ((unpackBool f) == (unpackBool s)))
                     | (onlyNumbers list) = (Bool ((unpackNum f) == (unpackNum s)))
                     | (onlyStrings list) = (Bool ((unpackString f) == (unpackString s)))
                     | otherwise = Error "Invalid parameters!"
equals a = Error "Invalid parameters!"

greater :: [LispVal] -> LispVal
greater list@(f:s:[]) | (onlyNumbers list) = (Bool ((unpackNum f) > (unpackNum s)))
                      | (onlyStrings list) = (Bool ((unpackString f) > (unpackString s)))
                      | otherwise = Error "Invalid parameters!"
greater a = Error "Invalid parameters!"

less :: [LispVal] -> LispVal
less list@(f:s:[]) | (onlyNumbers list) = (Bool ((unpackNum f) < (unpackNum s)))
                   | (onlyStrings list) = (Bool ((unpackString f) < (unpackString s)))
                   | otherwise = Error "Invalid parameters!"
less a = Error "Invalid parameters!"

greaterOrEquals :: [LispVal] -> LispVal
greaterOrEquals list@(f:s:[]) | (onlyNumbers list) = (Bool ((unpackNum f) >= (unpackNum s)))
                              | (onlyStrings list) = (Bool ((unpackString f) >= (unpackString s)))
                              | otherwise = Error "Invalid parameters!"
greaterOrEquals a = Error "Invalid parameters!"

lessOrEquals :: [LispVal] -> LispVal
lessOrEquals list@(f:s:[]) | (onlyNumbers list) = (Bool ((unpackNum f) <= (unpackNum s)))
                           | (onlyStrings list) = (Bool ((unpackString f) <= (unpackString s)))
                           | otherwise = Error "Invalid parameters!"
lessOrEquals a = Error "Invalid parameters!"

it :: [LispVal] -> LispVal
it list@(f:s:[]) | (onlyNumbers list) = (Bool ((unpackNum f) < (unpackNum s)))
                 | otherwise = Error "Invalid parameters!"
it a = Error "Invalid parameters!"

division :: [LispVal] -> LispVal
division list@(f:s:[]) | (onlyNumbers list) && ((unpackNum s) /= 0) = Number ((unpackNum f) `div` (unpackNum s))
                       | (onlyNumbers list) = Error "Can't divide by zero!"
                       | otherwise = Error "Invalid parameters!"
division a = Error "Invalid parameters!"

modulo :: [LispVal] -> LispVal
modulo list@(f:s:[]) | (onlyNumbers list) = Number ((unpackNum f) `mod` (unpackNum s))
                     | otherwise = Error "Invalid parameters!"
modulo a = Error "Invalid parameters!"

comment :: [LispVal] -> LispVal
comment list = List []

isList :: LispVal -> Bool
isList (List a) = True
isList a = False

unpackList :: LispVal -> [LispVal]
unpackList (List list) = list

cons :: [LispVal] -> LispVal
cons par@(a:list:[]) | (isList list) = List ([a] ++ (unpackList list))
                            | otherwise = Error "Invalid parameters!"
cons a = Error "Invalid parameters!"

compareList :: [LispVal] -> [LispVal] -> LispVal
compareList [] [] = (Bool True)
compareList [] (b:bs) = (Bool False)
compareList (b:bs) [] = (Bool False)
compareList (a:as) (b:bs) | (unpackBool (equals ([a]++[b]))) = (compareList as bs)
                          | otherwise = (Bool False)

eqv :: [LispVal] -> LispVal
eqv list@((Bool a):(Bool b):[]) = Bool (a==b)
eqv list@((List []):(List []):[]) = Bool True
eqv list@((String a):(String b):[]) = Bool (a==b)
eqv list@((Number a):(Number b):[]) = Bool (a==b)
eqv list@((List a):(List b):[]) = (compareList a b)
eqv list@((DottedList as a):(DottedList bs b):[]) = (Bool ((unpackBool (compareList as bs)) && (unpackBool (equals ([a]++[b])))))
eqv a = Error "Invalid parameters!"

-----------------------------------------------------------
--                     main FUNCTION                     --
-----------------------------------------------------------

showResult :: (LispVal, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer LispVal -> (LispVal, StateT)
getResult (ST f) = f empty -- we start with an empty state. 

main :: IO ()
main = do args <- getArgs
          putStr $ showResult $ getResult $ eval environment $ readExpr $ concat $ args 
          
