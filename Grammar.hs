module Grammar where

type Name = String

-- Aritmetic operations
data ArithOp = Add | Sub | Mult | Div

-- Boolean operations
data BoolOp = Not | GT | LT | GTE | LTE

-- List operations, categorized by whether they're unary or binary (if they take one list or two lists)
data ListOpUn = Map Expr | Ind Int
data ListOpBi = ListConcat

data Expr = If Expr Expr Expr               -- (if boolean-expr then-expr else-expr)
          | B Bool 
          | I Int 
          | N Name 
          | S String
          | StrConcat Expr Expr             -- (++ string-expr string-expr)
          | ArithExpr ArithOp Expr Expr     -- ([+, -, *, /] num-expr num-expr)
          | BoolExpr BoolOp Expr Expr       -- ([<, >, <=, >=] bool-expr bool-expr)
          | Let [(Expr, Expr)] Expr         -- (let ((name-expr val-expr) (name-expr val-expr) (name-expr val-expr) ...) (body-expr))
          | List [Expr]                     -- (list expr1 expr2 expr3 ...)
          | ListExprUn ListOpUn Expr        -- ([map <function>, ind <index value>] (list expr1 expr2 ...))
          | ListExprBi ListOpBi Expr Expr   -- (++ (list expr1 expr2 ...) (list expr3 expr4 ...))
          | Call Expr [Expr]                -- (function-name param1 param2 param3 ...)

data Stmt = Func Expr [Expr] Expr           -- (fn name-expr (param-expr1 param-expr2 ...) (body-expr))

data Prog = Prog [Stmt] Expr                -- A list of function definitions and a call to a function called "main"