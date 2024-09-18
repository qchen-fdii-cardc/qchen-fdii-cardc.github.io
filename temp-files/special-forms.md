# Symbol list

1. [SPECIAL-OPERATOR-P](#special-operator-p)
2. [CONSTANTP](#constantp)


## `SPECIAL-OPERATOR-P`

```lisp
COMMON-LISP:SPECIAL-OPERATOR-P
  [symbol]

SPECIAL-OPERATOR-P names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL) (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (SYMBOL) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    If the symbol globally names a special form, return T, otherwise NIL.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;MACROEXPAND.LISP

```

## `CONSTANTP`

```lisp
COMMON-LISP:CONSTANTP
  [symbol]

CONSTANTP names a compiled function:
  Lambda-list: (FORM &OPTIONAL (ENVIRONMENT NIL ENVP))
  Declared type: (FUNCTION
                  (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    True of any FORM that has a constant value: self-evaluating objects,
    keywords, defined constants, quote forms. Additionally the
    constant-foldability of some function calls and special forms is recognized.
    If ENVIRONMENT is provided, the FORM is first macroexpanded in it.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;COMPILER;EARLY-CONSTANTP.LISP

```


