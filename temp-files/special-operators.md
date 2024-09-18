# Symbol list

1. [LET*](#let*)
2. [RETURN-FROM](#return-from)
3. [SYMBOL-MACROLET](#symbol-macrolet)
4. [FUNCTION](#function)
5. [EVAL-WHEN](#eval-when)
6. [SETQ](#setq)
7. [THROW](#throw)
8. [THE](#the)
9. [ASSERT](#assert)
10. [FLET](#flet)
11. [MACROLET](#macrolet)
12. [MULTIPLE-VALUE-PROG1](#multiple-value-prog1)
13. [LET](#let)
14. [LOAD-TIME-VALUE](#load-time-value)
15. [PROGV](#progv)
16. [IF](#if)
17. [GO](#go)
18. [CATCH](#catch)
19. [UNWIND-PROTECT](#unwind-protect)
20. [LOCALLY](#locally)
21. [MULTIPLE-VALUE-CALL](#multiple-value-call)
22. [TAGBODY](#tagbody)
23. [QUOTE](#quote)
24. [LABELS](#labels)
25. [BLOCK](#block)
26. [PROGN](#progn)


## `LET*`

```lisp
COMMON-LISP:LET*
  [symbol]

LET* names a special operator:
  Lambda-list: (BINDINGS &BODY BODY)
  Documentation:
    LET* ({(var [value]) | var}*) declaration* form*
    
    Similar to LET, but the variables are bound sequentially, allowing each VALUE
    form to reference any of the previous VARS.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `RETURN-FROM`

```lisp
COMMON-LISP:RETURN-FROM
  [symbol]

RETURN-FROM names a special operator:
  Lambda-list: (NAME &OPTIONAL VALUE)
  Documentation:
    RETURN-FROM name value
    
    Evaluate the VALUE, returning its values from the lexically enclosing
    block NAME. This is constrained to be used only within the dynamic
    extent of the block.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `SYMBOL-MACROLET`

```lisp
COMMON-LISP:SYMBOL-MACROLET
  [symbol]

SYMBOL-MACROLET names a special operator:
  Lambda-list: (MACROBINDINGS &BODY BODY)
  Documentation:
    SYMBOL-MACROLET ({(name expansion)}*) decl* form*
    
    Define the NAMES as symbol macros with the given EXPANSIONS. Within the
    body, references to a NAME will effectively be replaced with the EXPANSION.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `FUNCTION`

```lisp
COMMON-LISP:FUNCTION
  [symbol]

FUNCTION names a special operator:
  Lambda-list: (THING)
  Documentation:
    FUNCTION name
    
    Return the lexically apparent definition of the function NAME. NAME may also
    be a lambda expression.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

FUNCTION names the system-class #<SB-PCL:SYSTEM-CLASS COMMON-LISP:FUNCTION>:
  Class precedence-list: FUNCTION, T
  Direct superclasses: T
  Direct subclasses: SB-KERNEL:INTERPRETED-FUNCTION,
                     SB-PCL::%METHOD-FUNCTION,
                     SB-PCL::STANDARD-FUNCALLABLE-INSTANCE,
                     SB-VM::CLOSURE-TRAMPOLINE, SB-FORMAT::FMT-CONTROL,
                     SB-PCL::CTOR, SB-MOP:FUNCALLABLE-STANDARD-OBJECT
  No direct slots.

FUNCTION names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (ARGS (QUOTE *)) (RESULT (QUOTE *)))

```

## `EVAL-WHEN`

```lisp
COMMON-LISP:EVAL-WHEN
  [symbol]

EVAL-WHEN names a special operator:
  Lambda-list: (SITUATIONS &REST FORMS)
  Documentation:
    EVAL-WHEN (situation*) form*
    
    Evaluate the FORMS in the specified SITUATIONS (any of :COMPILE-TOPLEVEL,
    :LOAD-TOPLEVEL, or :EXECUTE, or (deprecated) COMPILE, LOAD, or EVAL).
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `SETQ`

```lisp
COMMON-LISP:SETQ
  [symbol]

SETQ names a special operator:
  Lambda-list: (&WHOLE SOURCE &REST THINGS)
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `THROW`

```lisp
COMMON-LISP:THROW
  [symbol]

THROW names a special operator:
  Lambda-list: (TAG RESULT)
  Documentation:
    THROW tag form
    
    Do a non-local exit, return the values of FORM from the CATCH whose tag is EQ
    to TAG.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `THE`

```lisp
COMMON-LISP:THE
  [symbol]

THE names a special operator:
  Lambda-list: (VALUE-TYPE FORM)
  Documentation:
    Specifies that the values returned by FORM conform to the VALUE-TYPE.
    
    CLHS specifies that the consequences are undefined if any result is
    not of the declared type, but SBCL treats declarations as assertions
    as long as SAFETY is at least 2, in which case incorrect type
    information will result in a runtime type-error instead of leading to
    eg. heap corruption. This is however expressly non-portable: use
    CHECK-TYPE instead of THE to catch type-errors at runtime. THE is best
    considered an optimization tool to inform the compiler about types it
    is unable to derive from other declared types.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

(SETF THE) has a complex setf-expansion:
  Lambda-list: (TYPE PLACE)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

## `ASSERT`

```lisp
COMMON-LISP:ASSERT
  [symbol]

ASSERT names a macro:
  Lambda-list: (TEST-FORM &OPTIONAL PLACES DATUM &REST ARGUMENTS)
  Documentation:
    Signals an error if the value of TEST-FORM is NIL. Returns NIL.
    
       Optional DATUM and ARGUMENTS can be used to change the signaled
       error condition and are interpreted as in (APPLY #'ERROR DATUM
       ARGUMENTS).
    
       Continuing from the signaled error using the CONTINUE restart will
       allow the user to alter the values of the SETFable locations
       specified in PLACES and then start over with TEST-FORM.
    
       If TEST-FORM is of the form
    
         (FUNCTION ARG*)
    
       where FUNCTION is a function (but not a special operator like
       CL:OR, CL:AND, etc.) the results of evaluating the ARGs will be
       included in the error report if the assertion fails.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

## `FLET`

```lisp
COMMON-LISP:FLET
  [symbol]

FLET names a special operator:
  Lambda-list: (DEFINITIONS &BODY BODY)
  Documentation:
    FLET ({(name lambda-list declaration* form*)}*) declaration* body-form*
    
    Evaluate the BODY-FORMS with local function definitions. The bindings do
    not enclose the definitions; any use of NAME in the FORMS will refer to the
    lexically apparent function definition in the enclosing environment.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `MACROLET`

```lisp
COMMON-LISP:MACROLET
  [symbol]

MACROLET names a special operator:
  Lambda-list: (DEFINITIONS &REST BODY)
  Documentation:
    MACROLET ({(name lambda-list form*)}*) body-form*
    
    Evaluate the BODY-FORMS in an environment with the specified local macros
    defined. NAME is the local macro name, LAMBDA-LIST is a DEFMACRO style
    destructuring lambda list, and the FORMS evaluate to the expansion.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `MULTIPLE-VALUE-PROG1`

```lisp
COMMON-LISP:MULTIPLE-VALUE-PROG1
  [symbol]

MULTIPLE-VALUE-PROG1 names a special operator:
  Lambda-list: (VALUES-FORM &REST FORMS)
  Documentation:
    MULTIPLE-VALUE-PROG1 values-form form*
    
    Evaluate VALUES-FORM and then the FORMS, but return all the values of
    VALUES-FORM.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `LET`

```lisp
COMMON-LISP:LET
  [symbol]

LET names a special operator:
  Lambda-list: (BINDINGS &BODY BODY)
  Documentation:
    LET ({(var [value]) | var}*) declaration* form*
    
    During evaluation of the FORMS, bind the VARS to the result of evaluating the
    VALUE forms. The variables are bound in parallel after all of the VALUES forms
    have been evaluated.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `LOAD-TIME-VALUE`

```lisp
COMMON-LISP:LOAD-TIME-VALUE
  [symbol]

LOAD-TIME-VALUE names a special operator:
  Lambda-list: (FORM &OPTIONAL READ-ONLY-P)
  Documentation:
    Arrange for FORM to be evaluated at load-time and use the value produced as
    if it were a constant. If READ-ONLY-P is non-NIL, then the resultant object is
    guaranteed to never be modified, so it can be put in read-only storage.
  Source file: SYS:SRC;COMPILER;LTV.LISP

```

## `PROGV`

```lisp
COMMON-LISP:PROGV
  [symbol]

PROGV names a special operator:
  Lambda-list: (VARS VALS &BODY BODY)
  Source file: SYS:SRC;COMPILER;IR2TRAN.LISP

```

## `IF`

```lisp
COMMON-LISP:IF
  [symbol]

IF names a special operator:
  Lambda-list: (TEST THEN &OPTIONAL ELSE)
  Documentation:
    IF predicate then [else]
    
    If PREDICATE evaluates to true, evaluate THEN and return its values,
    otherwise evaluate ELSE and return its values. ELSE defaults to NIL.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `GO`

```lisp
COMMON-LISP:GO
  [symbol]

GO names a special operator:
  Lambda-list: (TAG)
  Documentation:
    GO tag
    
    Transfer control to the named TAG in the lexically enclosing TAGBODY. This is
    constrained to be used only within the dynamic extent of the TAGBODY.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `CATCH`

```lisp
COMMON-LISP:CATCH
  [symbol]

CATCH names a special operator:
  Lambda-list: (TAG &BODY BODY)
  Documentation:
    CATCH tag form*
    
    Evaluate TAG and instantiate it as a catcher while the body forms are
    evaluated in an implicit PROGN. If a THROW is done to TAG within the dynamic
    scope of the body, then control will be transferred to the end of the body and
    the thrown values will be returned.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `UNWIND-PROTECT`

```lisp
COMMON-LISP:UNWIND-PROTECT
  [symbol]

UNWIND-PROTECT names a special operator:
  Lambda-list: (PROTECTED &BODY CLEANUP)
  Documentation:
    UNWIND-PROTECT protected cleanup*
    
    Evaluate the form PROTECTED, returning its values. The CLEANUP forms are
    evaluated whenever the dynamic scope of the PROTECTED form is exited (either
    due to normal completion or a non-local exit such as THROW).
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `LOCALLY`

```lisp
COMMON-LISP:LOCALLY
  [symbol]

LOCALLY names a special operator:
  Lambda-list: (&BODY BODY)
  Documentation:
    LOCALLY declaration* form*
    
    Sequentially evaluate the FORMS in a lexical environment where the
    DECLARATIONS have effect. If LOCALLY is a top level form, then the FORMS are
    also processed as top level forms.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `MULTIPLE-VALUE-CALL`

```lisp
COMMON-LISP:MULTIPLE-VALUE-CALL
  [symbol]

MULTIPLE-VALUE-CALL names a special operator:
  Lambda-list: (FUN &REST ARGS)
  Documentation:
    MULTIPLE-VALUE-CALL function values-form*
    
    Call FUNCTION, passing all the values of each VALUES-FORM as arguments,
    values from the first VALUES-FORM making up the first argument, etc.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `TAGBODY`

```lisp
COMMON-LISP:TAGBODY
  [symbol]

TAGBODY names a special operator:
  Lambda-list: (&REST STATEMENTS)
  Documentation:
    TAGBODY {tag | statement}*
    
    Define tags for use with GO. The STATEMENTS are evaluated in order, skipping
    TAGS, and NIL is returned. If a statement contains a GO to a defined TAG
    within the lexical scope of the form, then control is transferred to the next
    statement following that tag. A TAG must be an integer or a symbol. A
    STATEMENT must be a list. Other objects are illegal within the body.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `QUOTE`

```lisp
COMMON-LISP:QUOTE
  [symbol]

QUOTE names a special operator:
  Lambda-list: (THING)
  Documentation:
    QUOTE value
    
    Return VALUE without evaluating it.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `LABELS`

```lisp
COMMON-LISP:LABELS
  [symbol]

LABELS names a special operator:
  Lambda-list: (DEFINITIONS &BODY BODY)
  Documentation:
    LABELS ({(name lambda-list declaration* form*)}*) declaration* body-form*
    
    Evaluate the BODY-FORMS with local function definitions. The bindings enclose
    the new definitions, so the defined functions can call themselves or each
    other.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `BLOCK`

```lisp
COMMON-LISP:BLOCK
  [symbol]

BLOCK names a special operator:
  Lambda-list: (NAME &REST FORMS)
  Documentation:
    BLOCK name form*
    
    Evaluate the FORMS as a PROGN. Within the lexical scope of the body,
    RETURN-FROM can be used to exit the form.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

## `PROGN`

```lisp
COMMON-LISP:PROGN
  [symbol]

PROGN names a special operator:
  Lambda-list: (&REST FORMS)
  Documentation:
    PROGN form*
    
    Evaluates each FORM in order, returning the values of the last form. With no
    forms, returns NIL.
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```


