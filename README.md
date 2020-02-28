# lil-lisp

#### 1. A list of all of your team members and their ONID usernames.

Sebastian Benjamin (benjamse)

Khuong Luu (luukh)

Phi Luu (luuph)


#### 2. A very brief (2-3 sentence) introduction to your language. This should include its name of your language, the language’s paradigm, and the most interesting features you decided to include.

Lil’ Lisp is a (little) version of the well-known Lisp programming language (specifically the Scheme dialect). Lil’ Lisp has a subset of features of Lisp without the IO. A small special thing about Lil’ Lisp is that, like Lisp, everything is an expression and the program eventually will be evaluated into a single expression which then will be evaluated into a value (of basic data type, a string, or a list)

#### 3. Instructions for how to execute example programs in your language. Specifically:

Our language implementation is intended to be run from GHCi. For now, our language implementation is not intended to be run from the command line (but this can change later).

Module to be loaded: Main

Note that we don't have a parser yet. These are just examples what the end product looks like. For more examples, please look at `Main.Test.hs` for tests for AST

A simple example run:

(Standing on the root directory of the project)

```
$ ghci
Prelude> :l Main
[1 of 2] Compiling Grammar          ( Grammar.hs, interpreted )
[2 of 2] Compiling Main                 ( Main.hs, interpreted )
Ok, two modules loaded.
*Main>
```

Good Examples:
Assume you have followed the instructions above which run GHCi and load the Main module

```
*Main> goodAddTwoInteger
Interpreting Program:
(+ 1 2)
-> 3
```


```
*Main> goodFibbo
Interpreting Program: 
(fn fib (n)
    (if (= n 0) 0
      (if (= n 1) 1
        (+ (call fib (- n 1)) (call fib (- n 2))))))
(fib 2)
-> 1
```


Bad Examples:
Assume you have followed the instructions above which run GHCi and load the Main module

```
*Main> badDivideByZero
Interpreting Program:
(/ 1 0)
-> Error: Divide by Zero
```

```
*Main> badConcat1
Interpreting Program:
(concatenate ‘string ‘False ‘“hey”)
-> Error: "Cannot concatenate non-strings"
```
