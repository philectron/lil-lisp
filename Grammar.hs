module Grammar where

type Name = String

-- Aritmetic operations
data ArithOp = Add | Sub | Mul
  deriving (Eq, Show)

-- Boolean operations
data BoolOp = Not | EQ | GT | LT | GTE | LTE
  deriving (Eq, Show)

-- List operations, categorized by whether they're unary or binary (if they take
-- one list or two lists)
data ListOpUn = Map Expr | Ind Int
  deriving (Eq, Show)
data ListOpBi = ListConcat
  deriving (Eq, Show)

data Expr = B Bool
          | I Int
          | N Name
          | S String
          | List [Expr]                     -- (list expr1 expr2 expr3 ...)
          | If Expr Expr Expr               -- (if boolean-expr then-expr else-expr)
          | StrConcat Expr Expr             -- (++ string-expr string-expr)
          | ArithExpr ArithOp Expr Expr     -- ([+, -, *, /] num-expr num-expr)
          | BoolExpr BoolOp Expr Expr       -- ([!, =, >, <, >=, <=] bool-expr bool-expr)
          | Let [(Expr, Expr)] Expr         -- (let ((name-expr val-expr) (name-expr val-expr) (name-expr val-expr) ...) (body-expr))
          | ListExprUn ListOpUn Expr        -- ([map <function>, ind <index value>] (list expr1 expr2 ...))
          | ListExprBi ListOpBi Expr Expr   -- (++ (list expr1 expr2 ...) (list expr3 expr4 ...))
          | Call Expr [Expr]                -- (call function-name (param1 param2 param3 ...))
  deriving (Eq, Show)

data Stmt = Func Expr [Expr] Expr           -- (fn name-expr (param-expr1 param-expr2 ...) (body-expr))
  deriving (Eq, Show)

data Prog = Prog [Stmt] Expr                -- A list of function definitions and a call to a function called "main"
  deriving (Eq, Show)
