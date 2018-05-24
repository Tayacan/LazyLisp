# Grammar for LazyLisp

    Program ::= Defs
    Defs    ::= Def Defs | ε
    Def     ::= FunctionDef | ValDef

    ValDef      ::= '(' 'val' name Expr ')'
    FunctionDef ::= '(' 'fun' name '(' FunType ')' '(' Parameter ')' Expr ')'
    Parameter   ::= name

    Type      ::= FunType | BasicType | '(' Type ')'

    FunType   ::= Type '->' Type

    ListType  ::= List Type
    BasicType ::= Int | Bool | Symbol | ListType

    Expr  ::= SimpleExpr | ListExpr | FunCall | CaseExpr | PrintExpr | LambdaExpr
    Exprs ::= Expr Exprs | ε

    SimpleExpr ::= integer | 'true' | 'false' | atom
    ListExpr   ::= '(' Exprs ')' | '(' 'cons' Expr Expr ')' | 'nil'
    FunCall    ::= '(' Expr Expr ')'
    CaseExpr   ::= '(' case Expr PatternExprs ')'
    PrintExpr  ::= '(' print Expr Expr ')'
    LambdaExpr ::= '(' 'lambda' '(' FunType ')' '(' Parameter ')' Expr ')'

    PatternExprs ::= PatternExpr PatternExprs | PatternExpr
    PatternExpr  ::= '(' Pattern Expr ')'

    Pattern     ::= BoolPattern | IntPattern | NamePattern | ListPattern
    BoolPattern ::= 'true' | 'false'
    IntPattern  ::= integer
    NamePattern ::= name
    ListPattern ::= 'nil' | '(' 'cons' Pattern Pattern ')'

## The `main` function

All programs must have a function called `main`, which serves as the entry point of the program. The `main` function has type `List Int -> Int`.

Example:

    (fun main (List Int -> Int) (xs)
      0)

This function ignores its input and returns 0.

## The `print` expression

The only way to get output is to use `print`, which is a built-in function that firsts prints the value of its first argument, then returns the value of its second argument.

Example:

    (fun main (List Int -> Int) (xs)
      (print (length xs)
             0))

    (fun length (List a -> Int) (xs)
      (case xs
        (nil 0)
        ((cons y ys) (+ 1 (length ys)))))

The `length` function recursively computes the length of a list. The `main` function prints the result of calling `length` on the input, and then returns 0.

The `length` function uses `case` and pattern matching to do its thing.

## Infinite lists

As the name implies, LazyLisp is lazily evaluated. That means we can have infinite lists:

    (fun main (List Int -> Int) (xs)
      (case xs
        ((cons n nil) (print (take 10 (infinite n)) 0))
        (any 1)))

    (fun inifinite (Int -> List Int) (x)
      (cons x (infinite x)))
    
    (fun take (Int -> List Int -> List Int) (n)
      (lambda (List Int -> List Int) (xs)
        (case n
          (0 nil)
          (m (case xs
            (nil nil)
            ((cons y ys) (cons y (take (- m 1) ys))))))))

Here, we use nested function definitions to define a function that takes two arguments (`take`). We can call it in `main` in the same way we call built-in functions
of multiple arguments, because function application is left-associative.
