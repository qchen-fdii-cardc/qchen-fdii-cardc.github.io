## 1AM external symbols


1. [*TESTS*](#*tests*)
2. [IS](#is)
3. [RUN](#run)
4. [SIGNALS](#signals)
5. [TEST](#test)


###  `*TESTS*`

```lisp
1AM:*TESTS*
  [symbol]

*TESTS* names a special variable:
  Value: NIL
  Documentation:
    A list of tests; the default argument to `run'.
```
###  `IS`

```lisp
1AM:IS
  [symbol]

IS names a macro:
  Lambda-list: (FORM)
  Documentation:
    Assert that `form' evaluates to non-nil.
  Source file: /home/qchen/quicklisp/dists/quicklisp/software/1am-20141106-git/1am.lisp
```
###  `RUN`

```lisp
1AM:RUN
  [symbol]

RUN names a compiled function:
  Lambda-list: (&OPTIONAL (TESTS *TESTS*))
  Derived type: (FUNCTION (&OPTIONAL T) (VALUES &OPTIONAL))
  Documentation:
    Run each test in the sequence `tests'. Default is `*tests*'.
  Source file: /home/qchen/quicklisp/dists/quicklisp/software/1am-20141106-git/1am.lisp
```
###  `SIGNALS`

```lisp
1AM:SIGNALS
  [symbol]

SIGNALS names a macro:
  Lambda-list: (CONDITION &BODY BODY)
  Documentation:
    Assert that `body' signals a condition of type `condition'.
  Source file: /home/qchen/quicklisp/dists/quicklisp/software/1am-20141106-git/1am.lisp
```
###  `TEST`

```lisp
1AM:TEST
  [symbol]

TEST names a macro:
  Lambda-list: (NAME &BODY BODY)
  Documentation:
    Define a test function and add it to `*tests*'.
  Source file: /home/qchen/quicklisp/dists/quicklisp/software/1am-20141106-git/1am.lisp
```
