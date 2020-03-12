# lil-lisp

#### 1. A list of all of your team members and their ONID usernames.

Sebastian Benjamin (benjamse)

Khuong Luu (luukh)

Phi Luu (luuph)


#### 2. A very brief (2-3 sentence) introduction to your language. This should include its name of your language, the language’s paradigm, and the most interesting features you decided to include.

Lil’ Lisp is a (little) version of the well-known Lisp programming language (specifically the Scheme dialect). Lil’ Lisp has a subset of features of Lisp without the IO. A small special thing about Lil’ Lisp is that, like Lisp, everything is an expression and the program eventually will be evaluated into a single expression which then will be evaluated into a value (of basic data type, a string, or a list)

#### 3. Instructions for how to execute example programs in your language. Specifically:

Our language implementation is intended to be run from GHCi.

Module to be loaded: Main

For more examples, please look at `Main.Test.hs` for tests for AST:

##### 3.1 To observe our prepared good and bad running examples, run:

```
$ doctest --verbose ./Main.Test.hs | less
```

This will output the detailed content of tests, along with our descriptions of the tests, the expected outputs, and the outputs. The whole output is paged with `less` pager for convenience.

##### 3.2. To manually test our program yourself

Open `ghci` and load the `Main` module:

(Standing on the root directory of the project)

```
$ ghci
Prelude> :l Main
[1 of 2] Compiling Grammar          ( Grammar.hs, interpreted )
[2 of 2] Compiling Main                 ( Main.hs, interpreted )
Ok, two modules loaded.
*Main>
```

Then, execute function `expr` with two arguments:

- First argument: the AST of the targeted language. Please refer to `Grammar.hs` for details about what a correct AST of **Lil' Lisp** looks like)

- Second argument: `[]` (this is the initial stack the program first starts with)

For example, let's say we'd like to test the interpreter on the following Lil' Lisp program

```
(define (ais3 a)
    (== a 3)
(ais3 3))
```

This Lil' Lisp program defines a function named `ais3` that return whether its first argument `a` is equal to integer `3` or not. Then, this program calls that same function with the first argument is `3`. The final output should be `True`.

The manual test should be (assuming you already loaded the `Main` module):

```
*Main> expr (Func "ais3" ["a"] (BoolExprBi Eq (Ref "a") (I 3)) (Call "ais3" [I 3])) []
B True
```

In this example:
- We tested three features of the interpreter at the same time: Function (`Func` and `Call`), binding (`Ref`), and boolean operation (`BoolExprBi`).

- The first argument is `(Func "ais3" ["a"] (BoolExprBi Eq (Ref "a") (I 3)) (Call "ais3" [I 3]))`

- The second argument is `[]` (always)

- The output is `B True`, which is what we expected. The interpreter has worked correctly.

For more examples of usage of `expr`, check out how we use it at the end of `Main.Test.hs`.
