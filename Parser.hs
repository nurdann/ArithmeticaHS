import Data.Char (isDigit, isAlpha, isAlphaNum)
import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

{- Grammar
Expression -> Term Expression'
Expression' -> + Term Expression'
            | - Term Expression'
Term -> Factor Term'
Term' -> * Factor Term'
        | / Factor Term'
Factor -> - Factor | ( Expression )  | Number
Number -> [0-9] { [0-9] }
-}

data Expr = Number Int 
    | Var String
    | Add Expr Expr 
    | Sub Expr Expr 
    | Neg  Expr 
    | Mult Expr Expr 
    | Div Expr Expr deriving Show

parse :: String -> Expr
parseExpr :: String -> Either String (Expr, String)
parseExprExt :: (Expr, String) -> Either String (Expr, String)
parseTerm :: String -> Either String (Expr, String)
parseTermExt :: (Expr, String) -> Either String (Expr, String)
parseFactor :: String -> Either String (Expr, String)
parseNumber :: String -> Either String (Expr, String)

parseExpr [] = Left "Empty expression"
parseExpr (x:xs) = case parseTerm (x:xs) of
    Right (lexpr, s@('+':ys)) -> parseExprExt (lexpr, s)
    Right (lexpr, s@('-':ys)) -> parseExprExt (lexpr, s)
    Right (expr, ys) -> Right (expr, ys)
    Left err -> Left err

parseExprExt (lexpr, '+':xs) = case parseTerm xs of
    Right (rexpr, []) -> Right (Add lexpr rexpr, [])
    Right (rexpr, ys) -> parseExprExt (Add lexpr rexpr, ys)
    Left err -> Left err
parseExprExt (lexpr, '-':xs) = case parseTerm xs of
    Right (rexpr, []) -> Right (Sub lexpr rexpr, [])
    Right (rexpr, ys) -> parseExprExt (Sub lexpr rexpr, ys)
    Left err -> Left err
parseExprExt (expr, xs) = Right (expr, xs)

parseTerm xs = case parseFactor xs of
    Right (lexpr, s@('*':ys)) -> parseTermExt (lexpr, s)
    Right (lexpr, s@('/':ys)) -> parseTermExt (lexpr, s)
    Right (expr, ys) -> Right (expr, ys)
    Left err -> Left err

--parseTermExt (expr, xs) | (trace $ show xs ++ show expr) False = undefined
parseTermExt (lexpr, '*':[]) = Left "Expected expression after (*)"
parseTermExt (lexpr, '*':xs) = case parseFactor xs of
    Right (rexpr, []) -> Right (Mult lexpr rexpr, [])
    Right (rexpr, ys) -> parseTermExt (Mult lexpr rexpr, ys)
    Left err -> Left err
parseTermExt (lexpr, '/':[]) = Left "Expected expression after (/)"
parseTermExt (lexpr, '/':xs) = case parseFactor xs of
    Right (rexpr, []) -> Right (Div lexpr rexpr, [])
    Right (rexpr, ys) -> parseTermExt (Div lexpr rexpr, ys)
    Left err -> Left err
parseTermExt (expr, xs) = Right (expr, xs)

parseFactor (' ':xs) = parseFactor xs
parseFactor ('\r':xs) = parseFactor xs
parseFactor ('\t':xs) = parseFactor xs
parseFactor ('\n':xs) = parseFactor xs
parseFactor ('(':xs) = case parseExpr xs of
    Right (expr, ')':ys) -> Right (expr, ys)
    Left err -> Left err
parseFactor ('-':xs) = case parseFactor xs of
    Right (expr, ys) -> Right (Neg expr, ys)
    Left err -> Left err
parseFactor xs = parseNumber xs

parseNumber [] = Left "Expected a non-empty input for a number"
parseNumber s@(x:xs)
    | isDigit x = Right (Number (read (fst numSpan) :: Int), snd numSpan)
    | isAlpha x = Right (Var (fst alphaSpan), snd alphaSpan)
    | otherwise = Left ("Expected a digit, instead got " ++ x:[])
    where 
        numSpan = span isDigit s
        alphaSpan = span isAlpha s

parse xs = case parseExpr xs of
    Right (expr, []) -> expr
    Right (expr, y:ys) -> error $ "Unparsed text left: " ++ (y:ys)
    Left err -> error $ "Error in parsing" ++ err

-- Extend with functions

data FuncDef = FuncDef String [String] Expr deriving Show
data FuncExpr = FuncExpr String [Expr] deriving Show


-- Parse function defition, e.g. f(x) = x + 3

parseFuncDef :: String -> Either String (FuncDef, String)
parseFuncName :: String -> Either String (String, String)
parseFuncArgs :: String -> Either String ([String], String)
parseFuncArg :: String -> Either String (String, String)
parseFuncArgsExt :: [String] -> String -> Either String ([String], String)

isName c =  isAlpha c || c == '\''

parseFuncDef [] = Left "Expected function definition"
parseFuncDef (x:xs) = case parseFuncName (x:xs) of
    Right (name@(y:ys), rest) -> case parseFuncArgs rest of
        Right (args, '=':rest) -> case parseExpr rest of
            --Right (expr, []) -> Right (FuncDef name args expr, [])
            Right (expr, zs) -> Right (FuncDef name args expr, zs)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

parseFuncName [] = Left "Expected function name"
parseFuncName (' ':xs) = parseFuncName xs
parseFuncName ('\n':xs) = parseFuncName xs
parseFuncName ('\r':xs) = parseFuncName xs
parseFuncName ('\t':xs) = parseFuncName xs
parseFuncName (x:xs) 
    | isAlpha x = Right (name, rest)
    | otherwise = Left ("First non-alphabetic character " ++ [x] ++ " at " ++ x:xs)
    where (name, rest) = span isName (x:xs)


parseFuncArgs (' ':xs) = parseFuncArgs xs
parseFuncArgs ('\n':xs) = parseFuncArgs xs
parseFuncArgs ('\r':xs) = parseFuncArgs xs
parseFuncArgs ('\t':xs) = parseFuncArgs xs
parseFuncArgs ('(':xs) = case parseFuncArg xs of
    Right (arg, ',':ys) -> parseFuncArgsExt [arg] ys
    Right (arg, ')':ys) -> Right ([arg], dropWhile (`elem` " \n\r\t") ys)
    Right (arg, ys) -> Left $ "Expected closing paranthesis ) at " ++ ys
    Left err -> Left err

parseFuncArg (' ':xs) = parseFuncArg xs
parseFuncArg ('\n':xs) = parseFuncArg xs
parseFuncArg ('\r':xs) = parseFuncArg xs
parseFuncArg ('\t':xs) = parseFuncArg xs
parseFuncArg [] = Left "Expected an argument"
parseFuncArg (x:xs) 
    | isAlpha x = Right (arg, rest)
    | otherwise = Left $ "Expected alphabet character for an argument at " ++ (x:xs)
    where (arg, rest) = span isName (x:xs)

parseFuncArgsExt args s = case parseFuncArg s of
    Right (arg, ',':ys) -> parseFuncArgsExt (args ++ [arg]) ys
    Right (arg, s@(')':ys)) -> Right (args ++ [arg], s)
    --Right (arg, []) -> Left "Expected argument termination either , or )"
    Left err -> Left err

-- Parse function expression, e.g. f(a + 3)

parseFuncExpr :: String -> Either String (FuncExpr, String)
parseFuncExpr [] = Left "Expected function expression"
parseFuncExpr (x:xs) = case parseFuncName (x:xs) of
    Right (name, '(':ys) -> case parseFuncExprArgs ys of
        Right (argExprs, ')':zs) -> Right (FuncExpr name argExprs, zs)
        Right (_, zs) -> Left "Expected function arguments termination with )"
        Left err -> Left err
    Right (_, ys) ->  Left "Expected argument beginning with ("
    Left err -> Left err

parseFuncExprArgs :: String -> Either String ([Expr], String)
parseFuncExprArgs xs = case parseExpr xs of
    Right (expr, ',':ys) -> parseFuncExprArgsExt [expr] ys
    Right (expr, ys) -> Right ([expr], ys)
    Left err -> Left err

parseFuncExprArgsExt :: [Expr] -> String -> Either String ([Expr], String)
parseFuncExprArgsExt exprs xs = case parseExpr xs of
    Right (expr, ',':ys) -> parseFuncExprArgsExt (exprs ++ [expr]) ys
    Right (expr, ys) -> Right (exprs ++ [expr], ys)
    Left err -> Left err

-- Evaluate the arithmetic expression
evalExpr :: Map.Map String Int -> Expr -> Int
evalExpr map (Add lexpr rexpr) = evalExpr map lexpr + evalExpr map rexpr
evalExpr map (Sub lexpr rexpr) = evalExpr map lexpr - evalExpr map rexpr
evalExpr map (Mult lexpr rexpr) = evalExpr map lexpr * evalExpr map rexpr
evalExpr map (Div lexpr rexpr) = evalExpr map lexpr `div` evalExpr map rexpr
evalExpr map (Neg expr) = negate (evalExpr map expr)
evalExpr map (Number x) = x
evalExpr map (Var s) = fromMaybe (error $ "Key " ++ s ++ " not found") (Map.lookup s map)

-- Create map from function expression
evalFuncExpr :: Map.Map String Int -> FuncDef -> [Expr] -> Int
evalFuncExpr map  (FuncDef name argNames expr) argExprs = 
    evalExpr (addArgs map argNames evaledArgs) expr
    where evaledArgs = evalFuncArgExprs map argExprs

evalFuncArgExprs :: Map.Map String Int -> [Expr] -> [Int]
evalFuncArgExprs map exprs = Prelude.map (evalExpr map) exprs

addArgs :: Map.Map String Int -> [String] -> [Int] -> Map.Map String Int
addArgs map [] [] = map
addArgs map (arg:argNames) (evaled:evaledArgs) = addArgs (Map.insert arg evaled map) argNames evaledArgs

-- Examples --
add3 = case parseFuncDef "f(x)=x+3" of
    Right (f, []) -> f
    Left err -> error err

callAdd3 = case parseFuncExpr "f(10)" of
    Right (FuncExpr _ args, []) -> evalFuncExpr Map.empty add3 args
    Left err -> error err

-- create map of function definitions
mapFunctions :: [FuncDef] -> Map.Map String FuncDef -> Map.Map String FuncDef
mapFunctions [] map = map
mapFunctions (f@(FuncDef name args expr): fs) map = mapFunctions fs (Map.insert name f map)

parseFuncDefList :: String -> [FuncDef] 
parseFuncDefList [] = []
parseFuncDefList s = case parseFuncDef s of
    Right (f, xs) -> f : parseFuncDefList xs
    Left err -> error err

evalFuncFromMap :: Map.Map String Int -> Map.Map String FuncDef -> FuncExpr -> Int
evalFuncFromMap varMap fmap (FuncExpr name args) = 
    case Map.lookup name fmap of
        Just f -> evalFuncExpr varMap f args
        Nothing -> error $ "No function named " ++ name

exFmaps = mapFunctions (parseFuncDefList "f(x)=x+3 g(z)=z*2 e(x)=x") Map.empty
ex0 fExprString = case parseFuncExpr fExprString of
    Right (fexpr, _) -> evalFuncFromMap Map.empty exFmaps fexpr

ex01 = ex0 "f(10)"