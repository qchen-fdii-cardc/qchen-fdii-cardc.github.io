# Symbol list

1. [*MACROEXPAND-HOOK*](#*macroexpand-hook*)


## `*MACROEXPAND-HOOK*`

```lisp
COMMON-LISP:*MACROEXPAND-HOOK*
  [symbol]

*MACROEXPAND-HOOK* names a special variable:
  Declared type: (OR FUNCTION SYMBOL)
  Declared always-bound.
  Value: FUNCALL
  Documentation:
    The value of this variable must be a designator for a function that can
      take three arguments, a macro expander function, the macro form to be
      expanded, and the lexical environment to expand in. The function should
      return the expanded form. This function is called by MACROEXPAND-1
      whenever a runtime expansion is needed. Initially this is set to
      FUNCALL.

```


