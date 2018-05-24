module LazyLisp.ADT where

import Data.Functor.Foldable

type Program = [Def]
type Name = String

data Def = FuncDef Name FuncTp Name Expr
         | ValDef Name Expr

data Tp = BTp BasicTp | FTp FuncTp
data BasicTp = IntTp | BoolTp | SymbolTp | ListTp Tp
type FuncTp = (Tp, Tp)

type Expr = Fix ExprF
data ExprF e = IntVal Int
             | TrueVal
             | FalseVal
             | Symbol Name
             | Nil
             | Cons e e
             | List [e]
             | FunCall e e
             | Case e [PatternExpr e]
             | Print e e
             | Lambda FuncTp Name e

type PatternExpr e = (Pattern, e)

data Pattern = TruePattern | FalsePattern
             | IntPattern Int
             | NamePattern Name
             | NilPattern
             | ConsPattern Pattern Pattern
