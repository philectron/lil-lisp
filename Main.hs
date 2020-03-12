module Main where

import Grammar

main :: IO ()
main = putStrLn "hello yes this is main"

-- Takes an expression and recursively evaluates until resulting in one of the
-- base cases.
expr :: Expr -> Env Expr -> Expr
expr (B v) e = B v
expr (I v) e = I v
expr (N v) e = N v
expr (S v) e = S v
expr (List v) e = List v
expr (Error v) e = S v
expr (C env params bodyExpr) e = C env params bodyExpr
expr (If cond exprThen exprElse) e = ifExpr (expr cond e) exprThen exprElse e
expr (StrConcat strl strr) e = strConcat (expr strl e) (expr strr e)
expr (ArithExpr op numl numr) e = arithExpr op (expr numl e) (expr numr e)
expr (BoolExprUn op bool) e = boolExprUn op (expr bool e)
expr (BoolExprBi op exprl exprr) e = boolExprBi op (expr exprl e) (expr exprr e)
expr (ListExprUn op list) e = listExprUn op (expr list e)
expr (ListExprBi op listl listr) e = listExprBi op (expr listl e) (expr listr e)
expr (Let defs body) e = letScope (evalBeforeBind defs e) body e
expr (Ref name) e = refExpr name e
expr (Func name params bodyExpr nextExpr) e = fnDef name params bodyExpr nextExpr e
expr (Call name arguments) e = fnCall name (evalArguments arguments e) e

-- Takes three expressions: bool-expr, expr-left, expr-right.
-- If bool-expr evaluates to true, returns expr-left.
-- Otherwise, returns expr-right.
--
-- Note: Due to performance and security reason,
--      we don't evaluate branches before we complete evaluating the condition expr
ifExpr :: Expr -> Expr -> Expr -> Env Expr -> Expr
ifExpr (B cond) exprThen exprElse env = if cond then (expr exprThen env) else (expr exprElse env)
ifExpr _ _ _ _ = Error "Cannot branch expressions based on non-boolean condition"

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
arithExpr Div (I numl) (I numr) = case numr of
                                    0 -> Error "Cannot perform division by zero"
                                    _ -> I $ div numl numr
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
boolExprUn Not (B bool) = B $ not bool
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
listExprUn (Ind i) (List v) = v !! i
listExprUn (Map n) (List v) = undefined
listExprUn _ _ = Error "Cannot map or index non-list type."

-- Takes an expression; if it's a list, perform the given operation on it.
-- Otherwise, throw an error.
listExprBi :: ListOpBi -> Expr -> Expr -> Expr
listExprBi ListConcat (List l) (List r) = List $ l ++ r
listExprBi _ _ _ = Error "Cannot concatenate non-list types with list concatenate operator."

-- Takes a list of expression tuples and an expression.
-- The list of expression tuples should be (Name, Value).
-- The expression can be anything -- it may optionally contain names.
-- This function returns the result of the computation in the expression, with
-- all the Names in the expression... replaced by the corrosponding values in =
-- the list of (Name, Value) tuples.
-- letScope defs body e
letScope :: [(Expr, Expr)] -> Expr -> Env Expr -> Expr
letScope [] _ _ = Error "Cannot bind an empty list"
letScope newBindings nextExpr currEnv =
    case (validateBindings newBindings) of
        True -> expr nextExpr (addBindings newBindings currEnv)
        False -> Error "The binding list is not valid"

-- Check if a list is of form [(N name, Expr)]
-- True if it does. False otherwise
validateBindings :: [(Expr, Expr)] -> Bool
validateBindings [] = True
validateBindings (x:xs) = case x of
                     (N n, _) -> validateBindings xs
                     _ -> False

-- Check for existing name in env before adding the new binding
-- Take: New binding, current env
-- Return: An new env with additional and/or updated bindings (if existing name is found)
addBindings :: Env Expr -> Env Expr -> Env Expr
addBindings [] [] = []
addBindings [] curEnv = curEnv
addBindings new [] = new
addBindings ((n,v):xs) env = case lookup n env of
                                  Just v' -> addBindings xs (replaceBinding (n,v) env)
                                  Nothing -> addBindings xs ((n,v):env)

-- Take: a binding in form of (name, newValue), and a current env
-- Return: a new env in which
--          the value of found name (from lookup) is replace with newValue
-- Precondition: there must be a binding whose name is found in the env (with lookup)
replaceBinding :: (Expr, Expr) -> Env Expr -> Env Expr
replaceBinding (n,v) ((n',v'):ys) = if n == n'
                                       then (n',v) : ys
                                       else (n',v') : (replaceBinding (n,v) ys)

-- Take: name of variable, an evironment
-- Return: Expr that was binded to that name in the given environment
--          or an error if that name was not binded in that environment
refExpr :: Expr -> Env Expr -> Expr
refExpr (N name) env = let foundBindings = filter (\(N n,v) -> n == name) env
                        in case foundBindings of
                            [] -> Error ("Value of '" ++ name ++ "' not found")
                            _ -> snd (head (foundBindings))
refExpr _ _ = Error "Must put name to refer a binded variable"

-- Evaluate the nextExpr with an environment in which
-- we've added a binding of the function name and its closure
-- params: functionName params functionBodyExpr nextExpr env
fnDef :: Expr -> [Expr] -> Expr -> Expr -> Env Expr -> Expr
fnDef (N fnName) params bodyExpr nextExpr e =
    case (isNamesList params) of
        True -> expr nextExpr (((N fnName), (C e params bodyExpr)) : e)
        False -> Error "Function parameters must be a list of strings"
fnDef _ _ _ _ _ = Error "Function's name must be a string"

-- Check if a [Expr] is a list that contains all (N n)'s
-- True if it does. False otherwise
isNamesList :: [Expr] -> Bool
isNamesList [] = True
isNamesList (x:xs) = case x of
                     (N n) -> isNamesList xs
                     _ -> False

-- Takes an expression and an expression list.
-- The first expression must be the name of a function defined with a Stmt.
-- The expression list can be any arbitrary values.
-- This function retuns the result of performing the computation defined by the
-- lreferenced function...using the values described in the argument list as
-- the passed-in parameters.
-- fnCall name-expr [expr1 expr2...]
fnCall :: Expr -> [Expr] -> Env Expr -> Expr
fnCall (N funcName) args e =
    let closure = (refExpr (N funcName) e) in case closure of
    (C fnEnv params bodyExpr)
        -> if length params /= length args
              then Error "Number of arguments does not match number of parameter"
           else expr bodyExpr e'
               where
                   e' = (bindedParams ++ fnEnv ++ fnNameBinding)
                   bindedParams = (matchPA params args e)
                   fnNameBinding = [(N funcName, closure)] -- for recursion
    _ -> Error ("Function " ++ funcName ++ " not found")
fnCall _ _ _ = Error "Function's name must be a string"

-- Takes 2 lists: list of variable names (1st list), list of expression (2nd list),
-- and an environment (3rd param)
-- For each member of 2nd list,
--      evaluate it with `expr` function within the given environment,
--      then bind it with the corresponding member of the 1st list,
--      then append it to the resulting env
matchPA :: [Expr] -> [Expr] -> Env Expr -> Env Expr
matchPA [] [] _ = []
matchPA (x:xs) (y:ys) e = (x,(expr y e)) : matchPA xs ys e

-- Take:
--      a list of bindings that are being added to the current environment
--      the current environment
-- Return: a list of bindings in which expressions are evaluated
evalBeforeBind :: [(Expr, Expr)] -> Env Expr -> [(Expr, Expr)]
evalBeforeBind [] _ = []
evalBeforeBind ((n,e):xs) currEnv = ((n, expr e currEnv) : (evalBeforeBind xs currEnv))

-- Take: a list of argument and an environment
-- Return: a list of argument in which
--      each member was evaluated within the given environment
evalArguments :: [Expr] -> Env Expr -> [Expr]
evalArguments [] _ = []
evalArguments (x:xs) e = (expr x e) : (evalArguments xs e)
