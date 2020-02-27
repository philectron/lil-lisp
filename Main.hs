module Main where

import Grammar

main :: IO ()
main = putStrLn "hello yes this is main"

-- Takes an expression and recursively evaluates until resulting in one of the
-- base cases.
expr :: Expr -> Expr
expr (B v) = B v
expr (I v) = I v
expr (N v) = N v
expr (S v) = S v
expr (List v) = List v
expr (Error v) = S v
expr (If cond exprl exprr) = ifExpr cond exprl exprr
expr (StrConcat strl strr) = strConcat strl strr
expr (ArithExpr op numl numr) = arithExpr op numl numr
expr (BoolExprUn op bool) = boolExprUn op bool
expr (BoolExprBi op exprl exprr) = boolExprBi op exprl exprr
expr (ListExprUn op list) = listExprUn op list
expr (ListExprBi op listl listr) = listExprBi op listl listr
expr (Call name arguments) = fnCall name arguments
expr (Let defs body) = letScope defs body

-- Takes three expressions: bool-expr, expr-left, expr-right.
-- If bool-expr evaluates to true, returns expr-left.
-- Otherwise, returns expr-right.
ifExpr :: Expr -> Expr -> Expr -> Expr
ifExpr = undefined

-- Takes two expressions: str-expr-l, str-expr-r.
-- If they're two strings, concatenates str-expr-l and str-expr-r and returns
-- the concatenated string.
-- Otherwise, throws an error.
strConcat :: Expr -> Expr -> Expr
strConcat (S strl) (S strr) = S $ strl ++ strr
strConcat _ _  = Error "Cannot concatenate non-strings"

-- Takes three expressions: arith-op, expr-l, expr-r.
-- If expr-l and expr-r are two integers, performs the given arithmetic
-- operation arith-op on them and returns the result.
-- Otherwise, throws an error.
arithExpr :: ArithOp -> Expr -> Expr -> Expr
arithExpr op (I numl) (I numr) = I $ operator numl numr
  where operator = case op of
                    Add -> (+)
                    Sub -> (-)
                    Mul -> (*)
arithExpr _ _ _ = Error "Cannot perform arithmetic operation on non-integers"

-- Takes two expressions: bool-un-op, bool-expr.
-- If the expression is a boolean, performs the given unary boolean operation
-- bool-un-op on it and returns the result.
-- Otherwise, throws an error.
boolExprUn :: BoolOpUn -> Expr -> Expr
boolExprUn op (B bool) = B $ operator bool
  where operator = case op of
                     Not -> (not)
boolExprUn _ _ = Error "Cannot perform unary boolean operation on non-booleans"

-- Takes three expressions: bool-bi-op, expr-l, expr-r.
-- If the expressions are both integers or both strings, performs the given
-- binary boolean operation bool-bi-op on them.
-- Otherwise, throws an error.
boolExprBi :: BoolOpBi -> Expr -> Expr -> Expr
boolExprBi op (I numl) (I numr) = B $ operator numl numr
  where operator = case op of
                     Eq  -> (==)
                     Gt  -> (>)
                     Lt  -> (<)
                     Gte -> (>=)
                     Lte -> (<=)
boolExprBi Eq (S strl) (S strr) = B $ (==) strl strr
boolExprBi _ (S strl) (S strr) = Error "Cannot perform inequality boolean operations on strings"
boolExprBi _ _ _ = Error "Cannot perform binary boolean operation on non-strings or non-integers or mismatched types"

-- Takes an expression; if it's a list, perform the given operation on it.
-- Otherwise, throw an error.
listExprUn :: ListOpUn -> Expr -> Expr
listExprUn = undefined

-- Takes an expression; if it's a list, perform the given operation on it.
-- Otherwise, throw an error.
listExprBi :: ListOpBi -> Expr -> Expr -> Expr
listExprBi ListConcat (List r) (List l) = List $ r ++ l
listExprBi _ _ _ = Error "Cannot concatenate non-list types with list concatenate operator."

-- Takes an expression and an expression list.
-- The first expression must be the name of a function defined with a Stmt.
-- The expression list can be any arbitrary values.
-- This function retuns the result of performing the computation defined by the
-- lreferenced function...using the values described in the argument list as
-- the passed-in parameters.
fnCall :: Expr -> [Expr] -> Expr
fnCall = undefined

-- Takes a list of expression tuples and an expression.
-- The list of expression tuples should be (Name, Value).
-- The expression can be anything -- it may optionally contain names.
-- This function returns the result of the computation in the expression, with
-- all the Names in the expression... replaced by the corrosponding values in =
-- the list of (Name, Value) tuples.
letScope :: [(Expr, Expr)] -> Expr -> Expr
letScope = undefined
