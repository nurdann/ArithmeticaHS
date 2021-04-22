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

### Haskell implementation

The data declaration follows from the definition,

```haskell
data Expr = Number Int 
    | Add Expr Expr 
    | Sub Expr Expr 
    | Neg  Expr 
    | Mult Expr Expr 
    | Div Expr Expr deriving Show
```
