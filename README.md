# Arithmetic sentence parser

## Grammar

Let's start with a simple grammar for evaluating arithmetic expressions using multiplication and addition only as follows, 

```
Expression -> Expression + Expression
            | Expression * Expression
            | ( Expression )
            | number
number -> [0-9] { [0-9] }
```

where capitalized words indicate a non-terminal expression and non-capitalized indicate terminal symbols.

However, the above grammar is not adequate because it does not prioritize the order of operations for the multiplication. We can modify the grammar so that whenever `+` operator is encountered, we look for multiplication operator.

```
Expression -> Expression + Term
            | Term
Term -> Term * Factor
        | Factor
Factor -> (Expression)
        | number
number -> [0-9] { [0-9] }
```
Source: https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/Syntax.html/node8.html

Let's try the above grammar on a string `3 + 2 * (1 + 6)`. We get 
```
(Expression 
    (Expression
        (Term 
            (Factor
                (number 3))))
    + 
    (Term 
        (Factor 
            (number 2))
        *
        (Factor
            (Expression
                (Expression
                    (Term 
                        (Factor (number 1))))
                +
                (Term
                    (Factor (number 6)))))))
```

Because the grammar has the following production `Expr -> Expr ...`, we can end up infinitely recursing the production because of left to right parsing of text. We can eliminate it by introducing symbols before the recursion so that each production consumes at least one symbol as follows,

```
Expression -> Term Expression'
Expression' -> + Term Expression' | Epsilon
Term -> Factor Term'
Term' -> * Factor Term' | Epsilon
Factor -> (Expression) | number
number -> [0-9] { [0-9] }
```
Source: https://en.wikipedia.org/wiki/Left_recursion

Let's add negation as well. Then, the full grammar for an arithmetic expression using subtraction and division is as follows,

```
Expression -> Term Expression'
Expression' -> + Term Expression'
            | - Term Expression'
Term -> Factor Term'
Term' -> * Factor Term'
        | / Factor Term'
Factor -> - Factor | ( Expression )  | number
number -> [0-9] { [0-9] }
```

### Haskell implementation (`ParserBasic.hs`)

The data declaration follows from the definition,

```haskell
data Expr = Number Int 
    | Add Expr Expr 
    | Sub Expr Expr 
    | Neg  Expr 
    | Mult Expr Expr 
    | Div Expr Expr deriving Show
```

This allows us to account for any possible production of grammar.

We will use `Either a b` data type that allows to return `Right b` for correct parsing of a text or `Left a` for an error message.

Haskell makes it intuitive to apply rules based on a grammar. In addition, we use a tuple `(Expr, String)` to keep track of parsed symbols and text to be parsed.

For example, the first rule that we need to apply is for `Term` in `Expression -> Term Expression'`,

```hs
parseExpr (x:xs) = case parseTerm (x:xs) of
    Right (lexpr, s@('+':ys)) -> parseExprExt (lexpr, s)
    Right (lexpr, s@('-':ys)) -> parseExprExt (lexpr, s)
    Right (expr, ys) -> Right (expr, ys)
    Left err -> Left err
```

Once, the term is extracted the proper symbol is consumed, `+` or `-`, and we look for another term. The reason we need extension `parseExprExt` is to start with a left expression and continually form a binary tree that represents an order of operations.

## Add functions (`ParserFunc.hs`)

We can add the following rules to to represent a function expression. `Factor` is modified to have `name` production for a variable name. We also need to add `FunctionExpression` as one of the productions of `Factor`, for cases such as `f(g(3) + 5)`.

```
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
```

**Note**: Curly braces indicate optional rule

We define function definition and expression to differentiate between creating a function, e.g. `f(a) = a + 3`, and evaluating it, e.g. `f(10)` or `f(f(10))`.

```hs
data FuncDef = FuncDef String [String] Expr deriving Show

data Expr = Number Int 
    | Var String
    | FuncExpr String [Expr] 
    | Add Expr Expr 
    | Sub Expr Expr 
    | Neg  Expr 
    | Mult Expr Expr 
    | Div Expr Expr deriving Show
```

The challenge is now to disambiguate between `name` and `FunctionExpression` because both start with an alphabetic letter. We can determine that by parsing for `FunctionExpression` and if unsuccessful backtract.

Use `parseDef` or `parse` so that whitespace is eliminated, otherwise other functions will error out.

Currently recursive functions will not evaluate because it will recursively call the function itself, e.g. 

```
> parseDef "f(a) = f(a-1)"
FuncDef "f" ["a"] (FuncExpr "f" [Sub (Var "a") (Number 1)])
```

Lastly, we add `main` function so that expressions can evaulated. We assign using `let` expression, e.g. `let a = parse text`, so that we can use `IO String` as `String` in our functions. For example,

```hs
main = do
    text <- getLine :: IO String
    let up = map toUpper text :: String
    putStrLn (up :: String)
```

We can run main inside the interpreter as follows
```
*Main> :main
"Enter expression, e.g. 3+3 or f(a)=a+2 or f(3)"
Press Ctrl-C to quit
λ> f(a) = a * 5
λ> f(10)
50
λ> ^CInterrupted.
```

Or compile using `ghc`

```
$ ghc -O2 Parser.hs
$ ./Parser
"Enter expression, e.g. 3+3 or f(a)=a+2 or f(3)"
Press Ctrl-C to quit
λ> f(a) = a + 3
λ> f(10)
13
λ> ^C
```