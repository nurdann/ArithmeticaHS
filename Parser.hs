import Data.Char (isDigit, isAlpha, isAlphaNum, toUpper)
import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
{- 
-- Base Grammar --

Expression -> Term Expression'
Expression' -> + Term Expression'
            | - Term Expression'
Term -> Factor Term'
Term' -> * Factor Term'
        | / Factor Term'
Factor -> - Factor | ( Expression )  | Number
Number -> [0-9] { [0-9] }


data Expr = Number Int 
    | Var String
    | Add Expr Expr 
    | Sub Expr Expr 
    | Neg  Expr 
    | Mult Expr Expr 
    | Div Expr Expr deriving Show
-}

{- 
-- Grammar with function expression --

Function -> name ( name {, name } ) = Expression
FunctionExpression -> name ( { Argument {, Argument } } )
Argument -> Expression | FunctionExpression

Expression -> Term Expression'
Expression' -> + Term Expression'
            | - Term Expression'

Term -> Factor Term'
Term' -> * Factor Term'
        | / Factor Term'

Factor -> - Factor | ( Expression )  | number | name | FunctionExpression
name -> [a-Z] { [a-Z'] }
number -> [0-9] { [0-9] }
-}


-- Extend with functions

data Expr = Number Int 
    | Var String
    | FuncExpr String [Expr] 
    | Add Expr Expr 
    | Sub Expr Expr 
    | Neg  Expr 
    | Mult Expr Expr 
    | Div Expr Expr deriving Show

data FuncDef = FuncDef String [String] Expr deriving Show

parse :: String -> Expr
parseExpr :: String -> Either String (Expr, String)
parseExprExt :: (Expr, String) -> Either String (Expr, String)
parseTerm :: String -> Either String (Expr, String)
parseTermExt :: (Expr, String) -> Either String (Expr, String)
parseFactor :: String -> Either String (Expr, String)
parseVar :: String -> Either String (Expr, String)
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
parseTermExt (lexpr, ['*']) = Left "Expected expression after (*)"
parseTermExt (lexpr, '*':xs) = case parseFactor xs of
    Right (rexpr, []) -> Right (Mult lexpr rexpr, [])
    Right (rexpr, ys) -> parseTermExt (Mult lexpr rexpr, ys)
    Left err -> Left err
parseTermExt (lexpr, ['/']) = Left "Expected expression after (/)"
parseTermExt (lexpr, '/':xs) = case parseFactor xs of
    Right (rexpr, []) -> Right (Div lexpr rexpr, [])
    Right (rexpr, ys) -> parseTermExt (Div lexpr rexpr, ys)
    Left err -> Left err
parseTermExt (expr, xs) = Right (expr, xs)

parseFactor ('(':xs) = case parseExpr xs of
    Right (expr, ')':ys) -> Right (expr, ys)
    Left err -> Left err
parseFactor ('-':xs) = case parseFactor xs of
    Right (expr, ys) -> Right (Neg expr, ys)
    Left err -> Left err
parseFactor s@(x:xs) 
    | isDigit x = parseNumber s
    | otherwise = case parseFuncName s of
        Right(name, '(':ys) -> case parseFuncExpr s of
            Right (fexpr, zs) -> Right (fexpr, zs)
            Left err -> Left err
        Right(_, _) -> parseVar s
        Left err -> Left err
        
parseVar [] = Left "Expected a non-empty input for a variable"
parseVar s@(x:xs) 
    | isAlpha x = Right (Var (fst alphaSpan), snd alphaSpan)
    | otherwise = Left ("Expected a variable, instead got " ++ [x] ++ " at " ++ (x:xs))
    where alphaSpan = span isAlpha s
    
parseNumber [] = Left "Expected a non-empty input for a number"
parseNumber s@(x:xs)
    | isDigit x = Right (Number (read (fst numSpan) :: Int), snd numSpan)
    | otherwise = Left ("Expected a digit, instead got " ++ [x] ++ " at " ++ (x:xs))
    where numSpan = span isDigit s

parse xs = case parseExpr (filter (not . (`elem` " \n\r\t")) xs) of
    Right (expr, []) -> expr
    Right (expr, y:ys) -> error $ "Unparsed text left: " ++ (y:ys)
    Left err -> error $ "Error in parsing" ++ err

--
-- Parse function defition, e.g. f(x) = x + 3
--

parseDef :: String -> FuncDef
parseFuncDef :: String -> Either String (FuncDef, String)
parseFuncName :: String -> Either String (String, String)
parseFuncArgs :: String -> Either String ([String], String)
parseFuncArg :: String -> Either String (String, String)
parseFuncArgsExt :: [String] -> String -> Either String ([String], String)

isName c =  isAlpha c || c == '\''

parseDef (x:xs) = case parseFuncDef (filter (not . (`elem` " \n\r\t")) (x:xs)) of
    Right (f, []) -> f
    Right (f, ys) -> error $ "Unparsed text left after function definition: " ++ ys
    Left err -> error err

parseFuncDef [] = Left "Expected function definition"
parseFuncDef (x:xs) = case parseFuncName (x:xs) of
    Right (name@(y:ys), rest) -> case parseFuncArgs rest of
        Right (args, '=':rest) -> case parseExpr rest of
            --Right (expr, []) -> Right (FuncDef name args expr, [])
            Right (expr, zs) -> Right (FuncDef name args expr, zs)
            Left err -> Left err
        Right (args, rest) -> Left $ "Expected an equal sign = before " ++ rest
        Left err -> Left err
    Left err -> Left err

parseFuncName [] = Left "Expected function name"
parseFuncName (x:xs) 
    | isAlpha x = Right (name, rest)
    | otherwise = Left ("First non-alphabetic character " ++ [x] ++ " at " ++ x:xs)
    where (name, rest) = span isName (x:xs)

parseFuncArgs ('(':')':xs) = Right ([], xs)
parseFuncArgs ('(':xs) = case parseFuncArg xs of
    Right (arg, ',':ys) -> parseFuncArgsExt [arg] ys
    Right (arg, ')':ys) -> Right ([arg], ys)
    Right (arg, ys) -> Left $ "Expected closing paranthesis ) at " ++ ys
    Left err -> Left err

parseFuncArg [] = Left "Expected an argument"
parseFuncArg (x:xs) 
    | isAlpha x = Right (arg, rest)
    | otherwise = Left $ "Expected alphabet character for an argument at " ++ (x:xs)
    where (arg, rest) = span isName (x:xs)

parseFuncArgsExt args s = case parseFuncArg s of
    Right (arg, ',':ys) -> parseFuncArgsExt (args ++ [arg]) ys
    Right (arg, ')':ys) -> Right (args ++ [arg], ys)
    --Right (arg, []) -> Left "Expected argument termination either , or )"
    Left err -> Left err

--
-- Parse function expression, e.g. f(a + 3)
--

parseFuncExpr :: String -> Either String (Expr, String)
parseFuncExpr [] = Left "Expected function expression"
parseFuncExpr (x:xs) = case parseFuncName (x:xs) of
    Right (name, '(':ys) -> case parseFuncExprArgs ys of
        Right (argExprs, ')':zs) -> Right (FuncExpr name argExprs, zs)
        Right (argExprs, zs) -> Left $ "Expected argument termination with ) at " ++ zs
        Left err -> Left err
    Right (_, ys) ->  Left $ "Expected argument beginning with ( at " ++ ys
    Left err -> Left err

parseFuncExprArgs :: String -> Either String ([Expr], String)
parseFuncExprArgs s@(')':_) = Right ([], s)
parseFuncExprArgs (x:xs) = case parseFuncExprArg (x:xs) of
    Right (argExpr, ',':ys) -> parseFuncExprArgsExt [argExpr] ys
    Right (argExpr, ys) -> Right ([argExpr], ys)
    Left err -> Left err
parseFuncExprArgs xs = Left $ "Expected argument list within () at " ++ xs

parseFuncExprArg :: String -> Either String (Expr, String)
parseFuncExprArg (x:xs) 
    | isAlpha x = case parseFuncName (x:xs) of
        Right (name, '(':ys) -> case parseFuncExpr (x:xs) of
            Right (fexpr, zs) -> Right (fexpr, zs)
            Left err -> Left err
        Right (name, ys) -> case parseExpr (x:xs) of
            Right (expr, zs) -> Right (expr, zs)
            Left err -> Left err
        Left err -> Left err
    | otherwise = case parseExpr (x:xs) of
        Right (expr, ys) -> Right (expr, ys)
        Left err -> Left err

parseFuncExprArgsExt :: [Expr] -> String -> Either String ([Expr], String)
parseFuncExprArgsExt args (x:xs) = case parseFuncExprArg (x:xs) of
    Right (expr, ',':ys) -> parseFuncExprArgsExt (args ++ [expr]) ys
    Right (expr, s@(')':ys)) -> Right (args ++ [expr], s)
    Right (expr, s) -> Left $ "Expected argument termination with ) or , at " ++ s
    Left err -> Left err

--
-- Evaluate the arithmetic expression
--

evalExpr :: Map.Map String FuncDef -> Map.Map String Int -> Expr -> Int
evalExpr fmap varMap (Add lexpr rexpr) = evalExpr  fmap varMap lexpr + evalExpr fmap varMap rexpr
evalExpr fmap varMap (Sub lexpr rexpr) = evalExpr fmap varMap lexpr - evalExpr fmap varMap rexpr
evalExpr fmap varMap (Mult lexpr rexpr) = evalExpr fmap varMap lexpr * evalExpr fmap varMap rexpr
evalExpr fmap varMap (Div lexpr rexpr) = evalExpr fmap varMap lexpr `div` evalExpr fmap varMap rexpr
evalExpr fmap varMap (Neg expr) = negate (evalExpr fmap varMap expr)
evalExpr fmap varMap (Number x) = x
evalExpr fmap varMap (Var s) = fromMaybe (error $ "Key " ++ s ++ " not found") (Map.lookup s varMap)
evalExpr fmap varMap (FuncExpr name argExprs) = evalFuncExpr fmap varMap fdef argExprs
    where fdef = fromMaybe (error $ "Function " ++ name ++ " is not defined") (Map.lookup name fmap)

evalFuncExpr :: Map.Map String FuncDef -> Map.Map String Int -> FuncDef -> [Expr] -> Int
evalFuncExpr fmap varMap (FuncDef name argNames expr) argExprs = 
    evalExpr fmap (addArgs varMap argNames evaledArgs) expr
        where evaledArgs = map (evalExpr fmap varMap) argExprs

addArgs :: Map.Map String Int -> [String] -> [Int] -> Map.Map String Int
addArgs map [] [] = map
addArgs map (arg:argNames) (evaled:evaledArgs) = addArgs (Map.insert arg evaled map) argNames evaledArgs
addArgs _ _ _ = error "Unequal number of arguments"

--
-- Helper functions
--

parseExprList :: [String] -> [Expr]
parseExprList = map parse

parseDefList :: [String] -> [FuncDef] 
parseDefList = map parseDef

mapDefinitions :: [FuncDef] -> Map.Map String FuncDef -> Map.Map String FuncDef
mapDefinitions [] fmap = fmap
mapDefinitions (f@(FuncDef name args expr): fs) fmap = mapDefinitions fs (Map.insert name f fmap)

--
-- Examples
--
p0 = map parseExpr ["f(3)", "g(f(5))", "f()", "e(0+r(20+3*m(5-3)))"]

-- Examples --
add3 = case parseFuncDef "f(x)=x+3" of
    Right (f, []) -> f
    Left err -> error err

callAdd3 = case parseExpr "f(10)" of
    Right (FuncExpr _ args, []) -> evalFuncExpr Map.empty Map.empty add3 args
    Left err -> error err

add100 = evalExpr (mapDefinitions baseFs Map.empty) Map.empty $ parse "add(add(3*5, 0), 100)"

baseFunctions = [
    "add(a, b) = a + b",
    "sub(a, b) = a - b",
    "mult(a, b) = a * b",
    "div(a, b) = a / b",
    "double(a) = a * a"
    ]

callExprs = [
    "add(add(3,5), 5)",
    "double(3) - 3",
    "mult(add(2, 1), double(2))"
    ]

--
-- Main interface start with base functions
--
baseFs = parseDefList baseFunctions
evalCallExprs = map (evalExpr (mapDefinitions baseFs Map.empty) Map.empty) parsedExprs == [13, 6, 12]
    where parsedExprs = parseExprList callExprs

isFuncDef s = case parseFuncDef (filter (not . (`elem` " \n\r\t")) s) of
    Right _ -> True
    Left err -> False

main = do
    let fmap = mapDefinitions baseFs Map.empty
    let varMap = Map.empty
    print "Enter expression, e.g. 3+3 or f(a)=a+2 or f(3)"
    putStrLn "Press Ctrl-C to quit"
    handler fmap varMap

handler fmap varMap = do
    putStr "\955> "
    hFlush stdout
    text <- getLine
    if isFuncDef text
        then handleFuncDefs fmap varMap text
        else handleExprs fmap varMap text

handleFuncDefs fmap varMap text = do
    let parsedDefText = parseDef text
    case parsedDefText of
        f@(FuncDef name _ _) -> handler (Map.insert name f fmap) varMap 

handleExprs fmap varMap text = do
    let parsedText = parse text
    print $ evalExpr fmap varMap parsedText
    handler fmap varMap