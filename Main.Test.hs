-- Prereq: Install doctest if needed
-- Usage: Run
--   doctest Main.Test

module Main.Test where

import Grammar
import Main

-- | Unit tests the semantics of the conditional expression.
--
--   >>> ifExprTest (B True) (I 5) (I 3)
--   I 5
--
--   >>> ifExprTest (B False) (I 5) (I 3)
--   I 3
--
--   >>> ifExprTest (B True) (S "abc") (I 123)
--   S "abc"
--
--   >>> ifExprTest (B False) (S "123") (B True)
--   B True
--
--   >>> ifExprTest (I 1) (S "123") (I 123)
--   Error "Cannot branch expressions based on non-boolean condition"
--
--   >>> ifExprTest (I 0) (S "123") (I 123)
--   Error "Cannot branch expressions based on non-boolean condition"
ifExprTest :: Expr -> Expr -> Expr -> Expr
ifExprTest = ifExpr

-- | Unit tests the semantics of the string concatenation expression.
--
--   >>> strConcatTest (S "foo") (S "bar")
--   S "foobar"
--
--   >>> strConcatTest (S "123") (S "456")
--   S "123456"
--
--   >>> strConcatTest (S "foo") (I 3)
--   Error "Cannot concatenate non-strings"
--
--   >>> strConcatTest (B False) (S "foo")
--   Error "Cannot concatenate non-strings"
--
--   >>> strConcatTest (I 3) (B True)
--   Error "Cannot concatenate non-strings"
strConcatTest :: Expr -> Expr -> Expr
strConcatTest = strConcat

-- | Unit tests the semantics of arithmetic expression.
--
--   >>> arithExprTest Add (I 5) (I 3)
--   I 8
--
--   >>> arithExprTest Sub (I 3) (I 5)
--   I (-2)
--
--   >>> arithExprTest Mul (I 4) (I 5)
--   I 20
--
--   >>> arithExprTest Add (S "123") (S "456")
--   Error "Cannot perform arithmetic operation on non-integers"
--
--   >>> arithExprTest Sub (I 123) (S "456")
--   Error "Cannot perform arithmetic operation on non-integers"
--
--   >>> arithExprTest Mul (B True) (S "456")
--   Error "Cannot perform arithmetic operation on non-integers"
arithExprTest :: ArithOp -> Expr -> Expr -> Expr
arithExprTest = arithExpr

-- | Unit tests the semantics of the unary boolean expression.
--
--   >>> boolExprUnTest Not (B True)
--   B False
--
--   >>> boolExprUnTest Not (B False)
--   B True
--
--   >>> boolExprUnTest Not (I 5)
--   Error "Cannot perform unary boolean operation on non-booleans"
--
--   >>> boolExprUnTest Not (S "True")
--   Error "Cannot perform unary boolean operation on non-booleans"
boolExprUnTest :: BoolOpUn -> Expr -> Expr
boolExprUnTest = boolExprUn

-- | Unit tests the semantics of binary boolean expressions.
--
--   >>> boolExprBiTest Eq (I 5) (I 5)
--   B True
--
--   >>> boolExprBiTest Eq (S "foo") (S "foo")
--   B True
--
--   >>> boolExprBiTest Gt (I 3) (I 2)
--   B True
--
--   >>> boolExprBiTest Lt (I 45) (I 43)
--   B False
--
--   >>> boolExprBiTest Gte (I 45) (I 43)
--   B True
--
--   >>> boolExprBiTest Lte (I 5) (I 5)
--   B True
--
--   >>> boolExprBiTest Lt (S "abc") (S "def")
--   Error "Cannot perform inequality boolean operations on strings"
--
--   >>> boolExprBiTest Eq (I 123) (S "123")
--   Error "Cannot perform binary boolean operation on non-strings or non-integers or mismatched types"
boolExprBiTest :: BoolOpBi -> Expr -> Expr -> Expr
boolExprBiTest = boolExprBi
