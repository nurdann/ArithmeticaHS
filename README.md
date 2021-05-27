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

We can add the following rules to to represent a function expression. `Factor` is modified to have `name` production for a variable name.
```
Function -> name ( Arguments ) = Expression
Arguments -> Expression {, Expression}
Factor -> - Factor | ( Expression )  | number | name
name -> [a-Z] { [a-Z'] }
```

We simply define function expression as
```hs
data FuncDef = FuncDef String [String] Expr
data FuncExpr = FuncExpr name [Expr]
```