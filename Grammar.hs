module Grammar where

type Name = String

-- Arithmetic operations
data ArithOp = Add | Sub | Mul
  deriving (Eq, Show)

-- Boolean operations, categorized by whether they're unary or binary (i.e. if
-- they take one boolean expression or two boolean expressions)
data BoolOpUn = Not
  deriving (Eq, Show)
data BoolOpBi = Eq | Gt | Lt | Gte | Lte
  deriving (Eq, Show)

-- List operations, categorized by whether they're unary or binary (i.e. if they
-- take one list or two lists)
data ListOpUn = Map Expr | Ind Int
  deriving (Eq, Show)
data ListOpBi = ListConcat
  deriving (Eq, Show)

-- | An environment maps variables to some type of values.
type Env t = [(Name,t)]

data Expr
  = B Bool                         -- base type: Boolean
  | I Int                          -- base type: Integer
  | N Name                         -- base type: Name
  | S String                       -- base type: String
  | List [Expr]                    -- (list expr1 expr2 ...)
  | Error String                   -- base type: Error
  | C (Env Expr) [Name] Expr       -- closure
  | If Expr Expr Expr              -- (if boolean-expr then-expr else-expr)
  | StrConcat Expr Expr            -- (++ string-expr-l string-expr-r)
  | ArithExpr ArithOp Expr Expr    -- ([+, -, *, /] num-expr-l num-expr-r)
  | BoolExprUn BoolOpUn Expr       -- (! bool-expr)
  | BoolExprBi BoolOpBi Expr Expr  -- ([=, >, <, >=, <=] expr-l expr-r)
  | ListExprUn ListOpUn Expr       -- ([map <function>, ind <index value>] (list expr1 expr2 ...))
  | ListExprBi ListOpBi Expr Expr  -- (++ (list expr1 expr2 ...) (list expr3 expr4 ...))
  | Let [(Name, Expr)] Expr        -- (let ((name-expr1 val-expr1) (name-expr2 val-expr2) ...) (body-expr))
  | Ref Name                       -- refer a binded name
  | Func Name [Name] Expr Expr     -- (define name-expr (param-expr1 param-expr2 ...) (next-expr))
  | Call Name [Expr]               -- (call function-name-expr (param-expr1 param-expr2 ...))
  deriving (Eq, Show)
