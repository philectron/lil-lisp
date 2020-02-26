module Main where

import Grammar

main :: IO ()
main = putStrLn "hello yes this is main"

-- Takes an expression and recursively evaluates until you have one of the 5 base types, or an error
expr :: Expr -> (Maybe Expr)
expr (B v) = Just (B v)
expr (I v) = Just (I v)
expr (N v) = Just (N v)
expr (S v) = Just (S v)
expr (List v) = Just (List v)
expr (StrConcat strl strr) = stringConcat strl strr
expr (ArithExpr op numl numr) = arithExpr op numl numr
expr (BoolExpr op booll boolr) = boolExpr op booll boolr
expr (ListExprUn op list) = listExprUn op list
expr (ListExprBi op listl listr) = listExprBi op listl listr
expr (Call name arguments) = fnCall name arguments
expr (Let defs body) = letScope defs body

-- Takes two expressions; if they're two strings, concat them. Otherwise, throw an error.
stringConcat :: Expr -> Expr -> (Maybe Expr)
stringConcat = undefined

-- Takes two expressions; if they're two numbers, perform the given operation on them. Otherwise, throw an error.
arithExpr :: ArithOp -> Expr -> Expr -> (Maybe Expr)
arithExpr = undefined

-- Takes two expressions; if they're two bools, perform the given operation on them. Otherwise, throw an error.
boolExpr :: BoolOp -> Expr -> Expr -> (Maybe Expr)
boolExpr = undefined

-- Takes an expression; if it's a list, perform the given operation on it. Otherwise, throw an error.
listExprUn :: ListOpUn -> Expr -> (Maybe Expr)
listExprUn = undefined

-- Takes an expression; if it's a list, perform the given operation on it. Otherwise, throw an error.
listExprBi :: ListOpBi -> Expr -> Expr -> (Maybe Expr)
listExprBi = undefined

-- Takes an expression and an expression list.
-- The first expression must be the name of a function defined with a Stmt.
-- The expression list can be any arbitrary values.
-- This function retuns the result of performing the computation defined by the referenced function...
-- ...using the values described in the argument list as the passed-in parameters.
fnCall :: Expr -> [Expr] -> (Maybe Expr)
fnCall = undefined

-- Takes a list of expression tuples and an expression.
-- The list of expression tuples should be (Name, Value).
-- The expression can be anything -- it may optionally contain names.
-- This function returns the result of the computation in the expression, with all the Names in the expression...
-- ... replaced by the corrosponding values in the list of (Name, Value) tuples.
letScope :: [(Expr, Expr)] -> Expr -> (Maybe Expr)
letScope = undefined