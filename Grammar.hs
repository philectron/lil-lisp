type Name = String

data ArithOp = Add | Sub | Mult | Div

data BoolOp = Not | GT | LT | GTE | LTE

data Expr = If Expr Expr Expr               -- (if boolean-expr then-expr else-expr)
          | B Bool 
          | I Int 
          | N Name 
          | S String
          | Concat Expr Expr                -- (++ string-expr string-expr)
          | ArithExpr ArithOp Expr Expr     -- ([+, -, *, /] num-expr num-expr)
          | BoolExpr BoolOp Expr Expr       -- ([<, >, <=, >=] bool-expr bool-expr)
          | Let [(Expr, Expr)] Expr         -- (let ((name-expr val-expr) (name-expr val-expr) (name-expr val-expr) ...) (body-expr))
          | List [Expr]                     -- (expr1 expr2 expr3 ...)
          | Call Stmt [Expr]                -- (function-name param1 param2 param3 ...)

data Stmt = Func Expr [Expr] Expr           -- (fn name-expr (param-expr1 param-expr2 ...) (body-expr))

data Prog = Prog [Stmt] Expr                -- A list of function definitions and a call to a function called "main"