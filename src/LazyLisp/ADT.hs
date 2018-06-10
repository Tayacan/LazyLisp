{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeFamilies #-}
module LazyLisp.ADT where

import Data.Functor.Foldable hiding (Nil, Cons)
import qualified Data.Text as T

type Program = [Def]
type Name = T.Text

data Def = FuncDef Name FuncTp Name Expr
         | ValDef Name Expr

data Tp = BTp BasicTp | FTp FuncTp | TypeVar Name
  deriving Show
data BasicTp = IntTp | BoolTp | ListTp Tp
  deriving Show
type FuncTp = (Tp, Tp)

type Expr = Fix ExprF
data ExprF e = IntVal Int
             | TrueVal
             | FalseVal
             | Ident Name
             | Nil
             | Cons e e
             | FunCall e e
             | Case e [PatternExpr e]
             | Print e e
             | Lambda FuncTp Name e
  deriving (Functor, Foldable, Traversable)

type instance Base Expr = ExprF

type PatternExpr e = (Pattern, e)

data Pattern = TruePattern | FalsePattern
             | IntPattern Int
             | NamePattern Name
             | NilPattern
             | ConsPattern Pattern Pattern

ppProg :: Program -> T.Text
ppProg = T.intercalate "\n\n" . map ppDef

ppDef :: Def -> T.Text
ppDef (FuncDef fname tp pname body) =
  T.concat ["(fun ", fname, "(", ppFuncTp tp, ") (", pname, ")", ppExpr body, ")"]
ppDef (ValDef name expr) =
  T.concat ["(val ", name, " ", ppExpr expr, ")"]

ppPattern :: Pattern -> T.Text
ppPattern TruePattern     = "true"
ppPattern FalsePattern    = "false"
ppPattern (IntPattern n)  = T.pack $ show n
ppPattern (NamePattern n) = n
ppPattern NilPattern      = "nil"
ppPattern (ConsPattern p1 p2) = T.concat ["(cons ", ppPattern p1, " ", ppPattern p2, ")"]

ppPatternExp :: PatternExpr T.Text -> T.Text
ppPatternExp (p, s) = T.concat ["(", ppPattern p, " ", s, ")"]

ppTp :: Tp -> T.Text
ppTp (BTp tp) = ppBasicTp tp
ppTp (FTp tp) = ppFuncTp tp
ppTp (TypeVar n) = n

ppBasicTp :: BasicTp -> T.Text
ppBasicTp IntTp = "Int"
ppBasicTp BoolTp = "Bool"
ppBasicTp (ListTp t) = T.concat ["(List ", ppTp t, ")"]

ppFuncTp :: FuncTp -> T.Text
ppFuncTp (t1, t2) = T.concat ["(", ppTp t1, " -> ", ppTp t2, ")"]

ppExprF :: ExprF T.Text -> T.Text
ppExprF (IntVal i)     = T.pack $ show i
ppExprF TrueVal        = "true"
ppExprF FalseVal       = "false"
ppExprF (Ident n)      = n
ppExprF Nil            = "nil"
ppExprF (Cons x xs)    = T.concat ["(cons ", x, " ", xs, ")"]
ppExprF (FunCall f x)  = T.concat ["(", f, " ", x, ")"]
ppExprF (Case x ps)    = T.concat ["(case ", x, " ", (T.intercalate " " . map ppPatternExp) ps, ")"]
ppExprF (Print x y)    = T.concat ["(print ", x, " ", y, ")"]
ppExprF (Lambda t n e) = T.concat ["(lambda ", ppFuncTp t, " (", n, ") ", e, ")"]

ppExpr :: Expr -> T.Text
ppExpr = cata ppExprF
