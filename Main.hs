module Main where

import Grammar

-- Result can be either an expression on success or an error message on failure.
type Result = Either Expr String

main :: IO ()
main = putStrLn "hello yes this is main"

-- Takes an expression and recursively evaluates until resulting in one of the 5
-- base types or an error.
expr :: Expr -> Result
expr (B v) = Left (B v)
expr (I v) = Left (I v)
expr (N v) = Left (N v)
expr (S v) = Left (S v)
expr (List v) = Left (List v)
expr (If cond exprl exprr) = ifExpr cond exprl exprr
expr (StrConcat strl strr) = stringConcat strl strr
expr (ArithExpr op numl numr) = arithExpr op numl numr
expr (BoolExpr op booll boolr) = boolExpr op booll boolr
expr (ListExprUn op list) = listExprUn op list
expr (ListExprBi op listl listr) = listExprBi op listl listr
expr (Call name arguments) = fnCall name arguments
expr (Let defs body) = letScope defs body

-- Takes three expressions: bool-expr, expr-left, and expr-right.
-- If bool-expr evaluates to true, returns expr-left.
-- Otherwise, returns expr-right.
ifExpr :: Expr -> Expr -> Expr -> Result
ifExpr = undefined

-- Takes two expressions; if they're two strings, concat them. Otherwise, throw
-- an error.
stringConcat :: Expr -> Expr -> Result
stringConcat (S strl) (S strr) = Left (S $ strl ++ strr)
stringConcat _ _  = Right "Cannot concatenate non-string types"

-- Takes two expressions; if they're two numbers, perform the given operation on
-- them. Otherwise, throw an error.
arithExpr :: ArithOp -> Expr -> Expr -> Result
arithExpr op (I numl) (I numr) = Left (I $ operator numl numr)
  where operator = case op of
                     Add -> (+)
                     Sub -> (-)
                     Mul -> (*)
arithExpr _ _ _ = Right "Cannot perform arithmetic operation on non-number types"

-- Takes two expressions; if they're two bools, perform the given operation on
-- them. Otherwise, throw an error.
boolExpr :: BoolOp -> Expr -> Expr -> Result
boolExpr = undefined

-- Takes an expression; if it's a list, perform the given operation on it.
-- Otherwise, throw an error.
listExprUn :: ListOpUn -> Expr -> Result
listExprUn = undefined

-- Takes an expression; if it's a list, perform the given operation on it.
-- Otherwise, throw an error.
listExprBi :: ListOpBi -> Expr -> Expr -> Result
listExprBi = undefined

-- Takes an expression and an expression list.
-- The first expression must be the name of a function defined with a Stmt.
-- The expression list can be any arbitrary values.
-- This function retuns the result of performing the computation defined by the
-- lreferenced function...using the values described in the argument list as
-- the passed-in parameters.
fnCall :: Expr -> [Expr] -> Result
fnCall = undefined

-- Takes a list of expression tuples and an expression.
-- The list of expression tuples should be (Name, Value).
-- The expression can be anything -- it may optionally contain names.
-- This function returns the result of the computation in the expression, with
-- all the Names in the expression... replaced by the corrosponding values in =
-- the list of (Name, Value) tuples.
letScope :: [(Expr, Expr)] -> Expr -> Result
letScope = undefined
