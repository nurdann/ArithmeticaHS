import Data.Char (isDigit)
{- Grammar
Expression -> Term Expression'
Expression' -> + Term Expression'
            | - Term Expression'
Term -> Factor Term'
Term' -> * Factor Term'
        | / Factor Term'
Factor -> - Factor | ( Expression )  | number
number -> [0-9] { [0-9] }
-}

data Expr = Number Int 
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

parseExpr (x:xs) = case parseTerm (x:xs) of
    Right (lexpr, s@('+':ys)) -> parseExprExt (lexpr, s)
    Right (lexpr, s@('-':ys)) -> parseExprExt (lexpr, s)
    Right (expr, ys) -> Right (expr, ys)
    Left err -> Left err

parseExprExt (lexpr, '+':xs) = case parseExpr xs of
    Right (rexpr, ys) -> Right (Add lexpr rexpr, ys)
    Left err -> Left err
parseExprExpt (lexpr, '-':xs) = case parseExpr xs of
    Right (rexpr, ys) -> Right (Sub lexpr rexpr, ys)
    Left err -> Left err

parseTerm xs = case parseFactor xs of
    Right (lexpr, s@('*':ys)) -> parseTermExt (lexpr, s)
    Right (lexpr, s@('/':ys)) -> parseTermExt (lexpr, s)
    Right (expr, ys) -> Right (expr, ys)
    Left err -> Left err

parseTermExt (lexpr, '*':[]) = Left "Expected expression after (*)"
parseTermExt (lexpr, '*':xs) = case parseTerm xs of
    Right (rexpr, ys) -> Right (Mult lexpr rexpr, ys)
    Left err -> Left err
parseTermExt (lexpr, '/':[]) = Left "Expected expression after (/)"
parseTermExt (lexpr, '/':xs) = case parseTerm xs of
    Right (rexpr, ys) -> Right (Div lexpr rexpr, ys)
    Left err -> Left err

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
    | isDigit x = Right (Number (read num :: Int), drop (length num) s)
    | otherwise = Left ("Expected a digit, instead got " ++ x:[])
    where num = takeWhile isDigit s

parse xs = case parseExpr xs of
    Right (expr, []) -> expr
    Right (expr, (y:ys)) -> error $ "Unparsed text left: " ++ (y:ys)
    Left err -> error $ "Error in parsing" ++ err

-- Evaluate the arithmetic expression
eval :: Expr -> Int
eval (Add lexpr rexpr) = eval lexpr + eval rexpr
eval (Sub lexpr rexpr) = eval lexpr - eval rexpr
eval (Mult lexpr rexpr) = eval lexpr * eval rexpr
eval (Div lexpr rexpr) = eval lexpr `div` eval rexpr
eval (Neg expr) = negate (eval expr)
eval (Number x) = x