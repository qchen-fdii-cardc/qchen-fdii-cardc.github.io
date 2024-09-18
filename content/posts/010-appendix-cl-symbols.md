+++
title = 'Appendix 001: Common Lisp Symbols分类参考'
date = 2024-09-18T12:24:43+08:00
draft = false
mathjax = false
categories = ['lisp']
tags = ['Common-Lisp', 'Lisp', 'Symbols', '参考手册', '符号分类参考']
toc = true
tocBorder = true
+++



## special variable

### Symbol list

1. [*](#*)
2. [**](#**)
3. [***](#***)
4. [*BREAK-ON-SIGNALS*](#*break-on-signals*)
5. [*COMPILE-FILE-PATHNAME*](#*compile-file-pathname*)
6. [*COMPILE-FILE-TRUENAME*](#*compile-file-truename*)
7. [*COMPILE-PRINT*](#*compile-print*)
8. [*COMPILE-VERBOSE*](#*compile-verbose*)
9. [*DEBUG-IO*](#*debug-io*)
10. [*DEBUGGER-HOOK*](#*debugger-hook*)
11. [*DEFAULT-PATHNAME-DEFAULTS*](#*default-pathname-defaults*)
12. [*ERROR-OUTPUT*](#*error-output*)
13. [*FEATURES*](#*features*)
14. [*GENSYM-COUNTER*](#*gensym-counter*)
15. [*LOAD-PATHNAME*](#*load-pathname*)
16. [*LOAD-PRINT*](#*load-print*)
17. [*LOAD-TRUENAME*](#*load-truename*)
18. [*LOAD-VERBOSE*](#*load-verbose*)
19. [*MACROEXPAND-HOOK*](#*macroexpand-hook*)
20. [*MODULES*](#*modules*)
21. [*PACKAGE*](#*package*)
22. [*PRINT-ARRAY*](#*print-array*)
23. [*PRINT-BASE*](#*print-base*)
24. [*PRINT-CASE*](#*print-case*)
25. [*PRINT-CIRCLE*](#*print-circle*)
26. [*PRINT-ESCAPE*](#*print-escape*)
27. [*PRINT-GENSYM*](#*print-gensym*)
28. [*PRINT-LENGTH*](#*print-length*)
29. [*PRINT-LEVEL*](#*print-level*)
30. [*PRINT-LINES*](#*print-lines*)
31. [*PRINT-MISER-WIDTH*](#*print-miser-width*)
32. [*PRINT-PPRINT-DISPATCH*](#*print-pprint-dispatch*)
33. [*PRINT-PRETTY*](#*print-pretty*)
34. [*PRINT-RADIX*](#*print-radix*)
35. [*PRINT-READABLY*](#*print-readably*)
36. [*PRINT-RIGHT-MARGIN*](#*print-right-margin*)
37. [*QUERY-IO*](#*query-io*)
38. [*RANDOM-STATE*](#*random-state*)
39. [*READ-BASE*](#*read-base*)
40. [*READ-DEFAULT-FLOAT-FORMAT*](#*read-default-float-format*)
41. [*READ-EVAL*](#*read-eval*)
42. [*READ-SUPPRESS*](#*read-suppress*)
43. [*READTABLE*](#*readtable*)
44. [*STANDARD-INPUT*](#*standard-input*)
45. [*STANDARD-OUTPUT*](#*standard-output*)
46. [*TERMINAL-IO*](#*terminal-io*)
47. [*TRACE-OUTPUT*](#*trace-output*)
48. [+](#+)
49. [++](#++)
50. [+++](#+++)
51. [-](#-)
52. [/](#/)
53. [//](#//)
54. [///](#///)


### `*`

```lisp
COMMON-LISP:*
  [symbol]

* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the value of the most recent top level EVAL

* names a compiled function:
  Lambda-list: (&REST NUMBERS)
  Declared type: (FUNCTION (&REST NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Return the product of its arguments. With no args, returns 1.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `**`

```lisp
COMMON-LISP:**
  [symbol]

** names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the previous value of *

```

### `***`

```lisp
COMMON-LISP:***
  [symbol]

*** names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the previous value of **

```

### `*BREAK-ON-SIGNALS*`

```lisp
COMMON-LISP:*BREAK-ON-SIGNALS*
  [symbol]

*BREAK-ON-SIGNALS* names a special variable:
  Declared type: (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS)
  Declared always-bound.
  Value: NIL
  Documentation:
    When (TYPEP condition *BREAK-ON-SIGNALS*) is true, then calls to SIGNAL will
       enter the debugger prior to signalling that condition.

```

### `*COMPILE-FILE-PATHNAME*`

```lisp
COMMON-LISP:*COMPILE-FILE-PATHNAME*
  [symbol]

*COMPILE-FILE-PATHNAME* names a special variable:
  Declared type: (OR PATHNAME NULL)
  Declared always-bound.
  Value: NIL
  Documentation:
    The defaulted pathname of the file currently being compiled, or NIL if not
      compiling.

```

### `*COMPILE-FILE-TRUENAME*`

```lisp
COMMON-LISP:*COMPILE-FILE-TRUENAME*
  [symbol]

*COMPILE-FILE-TRUENAME* names a special variable:
  Declared type: (OR PATHNAME NULL)
  Declared always-bound.
  Value: NIL
  Documentation:
    The TRUENAME of the file currently being compiled, or NIL if not
      compiling.

```

### `*COMPILE-PRINT*`

```lisp
COMMON-LISP:*COMPILE-PRINT*
  [symbol]

*COMPILE-PRINT* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    The default for the :PRINT argument to COMPILE-FILE.

```

### `*COMPILE-VERBOSE*`

```lisp
COMMON-LISP:*COMPILE-VERBOSE*
  [symbol]

*COMPILE-VERBOSE* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: T
  Documentation:
    The default for the :VERBOSE argument to COMPILE-FILE.

```

### `*DEBUG-IO*`

```lisp
COMMON-LISP:*DEBUG-IO*
  [symbol]

*DEBUG-IO* names a special variable:
  Declared type: STREAM
  Declared always-bound.
  Value: #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10000227D3}>
  Documentation:
    interactive debugging stream

```

### `*DEBUGGER-HOOK*`

```lisp
COMMON-LISP:*DEBUGGER-HOOK*
  [symbol]

*DEBUGGER-HOOK* names a special variable:
  Declared type: (OR FUNCTION SYMBOL)
  Declared always-bound.
  Value: #<FUNCTION (LAMBDA (ALIVE/SESSION::C ALIVE/SESSION::H)
                      :IN
                      ALIVE/SESSION::RUN-FN) {1003EE01DB}>
  Documentation:
    This is either NIL or a function of two arguments, a condition and the value
       of *DEBUGGER-HOOK*. This function can either handle the condition or return
       which causes the standard debugger to execute. The system passes the value
       of this variable to the function because it binds *DEBUGGER-HOOK* to NIL
       around the invocation.

```

### `*DEFAULT-PATHNAME-DEFAULTS*`

```lisp
COMMON-LISP:*DEFAULT-PATHNAME-DEFAULTS*
  [symbol]

*DEFAULT-PATHNAME-DEFAULTS* names a special variable:
  Declared type: PATHNAME
  Declared always-bound.
  Value: #P"c:/PARA/0_Projects/tech-blog/qchen-fdii-cardc.github.io/"

```

### `*ERROR-OUTPUT*`

```lisp
COMMON-LISP:*ERROR-OUTPUT*
  [symbol]

*ERROR-OUTPUT* names a special variable:
  Declared type: STREAM
  Declared always-bound.
  Value: #<ALIVE/SBCL/STREAMS:IO-STREAM {1003EE8223}>
  Documentation:
    error output stream

```

### `*FEATURES*`

```lisp
COMMON-LISP:*FEATURES*
  [symbol]

*FEATURES* names a special variable:
  Declared type: LIST
  Declared always-bound.
  Value: (:FLEXI-STREAMS :BORDEAUX-THREADS :WINDOWS :GLOBAL-VARS
          ALEXANDRIA::SEQUENCE-EMPTYP :SPLIT-SEQUENCE :THREAD-SUPPORT
          :CL-JSON-CLOS :CL-JSON :QUICKLISP :ASDF3.3 :ASDF3.2 :ASDF3.1
          :ASDF3 :ASDF2 :ASDF :OS-WINDOWS :NON-BASE-CHARS-EXIST-P
          :ASDF-UNICODE :ARENA-ALLOCATOR :X86-64 :GENCGC :64-BIT
          :ANSI-CL :COMMON-LISP :IEEE-FLOATING-POINT :LITTLE-ENDIAN
          :PACKAGE-LOCAL-NICKNAMES :SB-LDB :SB-PACKAGE-LOCKS
          :SB-SAFEPOINT :SB-THREAD :SB-UNICODE :SBCL :WIN32)
  Documentation:
    a list of symbols that describe features provided by the
       implementation

```

### `*GENSYM-COUNTER*`

```lisp
COMMON-LISP:*GENSYM-COUNTER*
  [symbol]

*GENSYM-COUNTER* names a special variable:
  Declared type: UNSIGNED-BYTE
  Declared always-bound.
  Value: 3395
  Documentation:
    counter for generating unique GENSYM symbols

```

### `*LOAD-PATHNAME*`

```lisp
COMMON-LISP:*LOAD-PATHNAME*
  [symbol]

*LOAD-PATHNAME* names a special variable:
  Declared type: (OR PATHNAME NULL)
  Declared always-bound.
  Value: NIL
  Documentation:
    the defaulted pathname that LOAD is currently loading

```

### `*LOAD-PRINT*`

```lisp
COMMON-LISP:*LOAD-PRINT*
  [symbol]

*LOAD-PRINT* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the default for the :PRINT argument to LOAD

```

### `*LOAD-TRUENAME*`

```lisp
COMMON-LISP:*LOAD-TRUENAME*
  [symbol]

*LOAD-TRUENAME* names a special variable:
  Declared type: (OR PATHNAME NULL)
  Declared always-bound.
  Value: NIL
  Documentation:
    the TRUENAME of the file that LOAD is currently loading

```

### `*LOAD-VERBOSE*`

```lisp
COMMON-LISP:*LOAD-VERBOSE*
  [symbol]

*LOAD-VERBOSE* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the default for the :VERBOSE argument to LOAD

```

### `*MACROEXPAND-HOOK*`

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

### `*MODULES*`

```lisp
COMMON-LISP:*MODULES*
  [symbol]

*MODULES* names a special variable:
  Declared type: LIST
  Declared always-bound.
  Value: ("SB-INTROSPECT" "SB-BSD-SOCKETS" "SB-POSIX" "ASDF" "asdf"
          "UIOP" "uiop")
  Documentation:
    This is a list of module names that have been loaded into Lisp so far.
       It is used by PROVIDE and REQUIRE.

```

### `*PACKAGE*`

```lisp
COMMON-LISP:*PACKAGE*
  [symbol]

*PACKAGE* names a special variable:
  Declared type: PACKAGE
  Declared always-bound.
  Value: #<PACKAGE "COMMON-LISP-USER">
  Documentation:
    the current package

```

### `*PRINT-ARRAY*`

```lisp
COMMON-LISP:*PRINT-ARRAY*
  [symbol]

*PRINT-ARRAY* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: T
  Documentation:
    Should the contents of arrays be printed?

```

### `*PRINT-BASE*`

```lisp
COMMON-LISP:*PRINT-BASE*
  [symbol]

*PRINT-BASE* names a special variable:
  Declared type: (INTEGER 2 36)
  Declared always-bound.
  Value: 10
  Documentation:
    The output base for RATIONALs (including integers).

```

### `*PRINT-CASE*`

```lisp
COMMON-LISP:*PRINT-CASE*
  [symbol]

*PRINT-CASE* names a special variable:
  Declared type: (MEMBER :CAPITALIZE :DOWNCASE :UPCASE)
  Declared always-bound.
  Value: :UPCASE
  Documentation:
    What case should the printer should use default?

```

### `*PRINT-CIRCLE*`

```lisp
COMMON-LISP:*PRINT-CIRCLE*
  [symbol]

*PRINT-CIRCLE* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: T
  Documentation:
    Should we use #n= and #n# notation to preserve uniqueness in general (and
      circularity in particular) when printing?

```

### `*PRINT-ESCAPE*`

```lisp
COMMON-LISP:*PRINT-ESCAPE*
  [symbol]

*PRINT-ESCAPE* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: T
  Documentation:
    Should we print in a reasonably machine-readable way? (possibly
      overridden by *PRINT-READABLY*)

```

### `*PRINT-GENSYM*`

```lisp
COMMON-LISP:*PRINT-GENSYM*
  [symbol]

*PRINT-GENSYM* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: T
  Documentation:
    Should #: prefixes be used when printing symbols with null SYMBOL-PACKAGE?

```

### `*PRINT-LENGTH*`

```lisp
COMMON-LISP:*PRINT-LENGTH*
  [symbol]

*PRINT-LENGTH* names a special variable:
  Declared type: (OR UNSIGNED-BYTE NULL)
  Declared always-bound.
  Value: NIL
  Documentation:
    How many elements at any level should be printed before abbreviating
      with "..."?

```

### `*PRINT-LEVEL*`

```lisp
COMMON-LISP:*PRINT-LEVEL*
  [symbol]

*PRINT-LEVEL* names a special variable:
  Declared type: (OR UNSIGNED-BYTE NULL)
  Declared always-bound.
  Value: NIL
  Documentation:
    How many levels should be printed before abbreviating with "#"?

```

### `*PRINT-LINES*`

```lisp
COMMON-LISP:*PRINT-LINES*
  [symbol]

*PRINT-LINES* names a special variable:
  Declared type: (OR UNSIGNED-BYTE NULL)
  Declared always-bound.
  Value: NIL
  Documentation:
    The maximum number of lines to print per object.

```

### `*PRINT-MISER-WIDTH*`

```lisp
COMMON-LISP:*PRINT-MISER-WIDTH*
  [symbol]

*PRINT-MISER-WIDTH* names a special variable:
  Declared type: (OR UNSIGNED-BYTE NULL)
  Declared always-bound.
  Value: NIL
  Documentation:
    If the remaining space between the current column and the right margin
       is less than this, then print using ``miser-style'' output. Miser
       style conditional newlines are turned on, and all indentations are
       turned off. If NIL, never use miser mode.

```

### `*PRINT-PPRINT-DISPATCH*`

```lisp
COMMON-LISP:*PRINT-PPRINT-DISPATCH*
  [symbol]

*PRINT-PPRINT-DISPATCH* names a special variable:
  Declared type: SB-PRETTY:PPRINT-DISPATCH-TABLE
  Declared always-bound.
  Value: #<SB-PRETTY:PPRINT-DISPATCH-TABLE {1000022163}>
  Documentation:
    The pprint-dispatch-table that controls how to pretty-print objects.

```

### `*PRINT-PRETTY*`

```lisp
COMMON-LISP:*PRINT-PRETTY*
  [symbol]

*PRINT-PRETTY* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: T
  Documentation:
    Should pretty printing be used?

```

### `*PRINT-RADIX*`

```lisp
COMMON-LISP:*PRINT-RADIX*
  [symbol]

*PRINT-RADIX* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    Should base be verified when printing RATIONALs?

```

### `*PRINT-READABLY*`

```lisp
COMMON-LISP:*PRINT-READABLY*
  [symbol]

*PRINT-READABLY* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    If true, all objects will be printed readably. If readable printing
      is impossible, an error will be signalled. This overrides the value of
      *PRINT-ESCAPE*.

```

### `*PRINT-RIGHT-MARGIN*`

```lisp
COMMON-LISP:*PRINT-RIGHT-MARGIN*
  [symbol]

*PRINT-RIGHT-MARGIN* names a special variable:
  Declared type: (OR UNSIGNED-BYTE NULL)
  Declared always-bound.
  Value: 72
  Documentation:
    The position of the right margin in ems (for pretty-printing).

```

### `*QUERY-IO*`

```lisp
COMMON-LISP:*QUERY-IO*
  [symbol]

*QUERY-IO* names a special variable:
  Declared type: STREAM
  Declared always-bound.
  Value: #<ALIVE/SBCL/STREAMS:IO-STREAM {1003EE8143}>
  Documentation:
    query I/O stream

```

### `*RANDOM-STATE*`

```lisp
COMMON-LISP:*RANDOM-STATE*
  [symbol]

*RANDOM-STATE* names a special variable:
  Declared type: RANDOM-STATE
  Declared always-bound.
  Value: #S(RANDOM-STATE :STATE #.(MAKE-ARRAY 627 :ELEMENT-TYPE
                                              '(UNSIGNED-BYTE 32)
                                              :INITIAL-CONTENTS
                                              '(0 2567483615 624 5489
                                                1301868182 2938499221
                                                2950281878 1875628136
                                                751856242 944701696
                                                2243192071 694061057
                                                219885934 2066767472
                                                3182869408 485472502
                                                2336857883 1071588843
                                                3418470598 951210697
                                                3693558366 2923482051
                                                1793174584 2982310801
                                                1586906132 1951078751
                                                1808158765 1733897588
                                                431328322 4202539044
                                                530658942 1714810322
                                                3025256284 3342585396
                                                1937033938 2640572511
                                                1654299090 3692403553
                                                4233871309 3497650794
                                                862629010 2943236032
                                                2426458545 1603307207
                                                1133453895 3099196360
                                                2208657629 2747653927
                                                931059398 761573964
                                                3157853227 785880413
                                                730313442 124945756
                                                2937117055 3295982469
                                                1724353043 3021675344
                                                3884886417 4010150098
                                                4056961966 699635835
                                                2681338818 1339167484
                                                720757518 2800161476
                                                2376097373 1532957371
                                                3902664099 1238982754
                                                3725394514 3449176889
                                                3570962471 4287636090
                                                4087307012 3603343627
                                                202242161 2995682783
                                                1620962684 3704723357
                                                371613603 2814834333
                                                2111005706 624778151
                                                2094172212 4284947003
                                                1211977835 991917094
                                                1570449747 2962370480
                                                1259410321 170182696
                                                146300961 2836829791
                                                619452428 2723670296
                                                1881399711 1161269684
                                                1675188680 4132175277
                                                780088327 3409462821
                                                1036518241 1834958505
                                                3048448173 161811569
                                                618488316 44795092
                                                3918322701 1924681712
                                                3239478144 383254043
                                                4042306580 2146983041
                                                3992780527 3518029708
                                                3545545436 3901231469
                                                1896136409 2028528556
                                                2339662006 501326714
                                                2060962201 2502746480
                                                561575027 581893337
                                                3393774360 1778912547
                                                3626131687 2175155826
                                                319853231 986875531
                                                819755096 2915734330
                                                2688355739 3482074849
                                                2736559 2296975761
                                                1029741190 2876812646
                                                690154749 579200347
                                                4027461746 1285330465
                                                2701024045 4117700889
                                                759495121 3332270341
                                                2313004527 2277067795
                                                4131855432 2722057515
                                                1264804546 3848622725
                                                2211267957 4100593547
                                                959123777 2130745407
                                                3194437393 486673947
                                                1377371204 17472727
                                                352317554 3955548058
                                                159652094 1232063192
                                                3835177280 49423123
                                                3083993636 733092
                                                2120519771 2573409834
                                                1112952433 3239502554
                                                761045320 1087580692
                                                2540165110 641058802
                                                1792435497 2261799288
                                                1579184083 627146892
                                                2165744623 2200142389
                                                2167590760 2381418376
                                                1793358889 3081659520
                                                1663384067 2009658756
                                                2689600308 739136266
                                                2304581039 3529067263
                                                591360555 525209271
                                                3131882996 294230224
                                                2076220115 3113580446
                                                1245621585 1386885462
                                                3203270426 123512128
                                                12350217 354956375
                                                4282398238 3356876605
                                                3888857667 157639694
                                                2616064085 1563068963
                                                2762125883 4045394511
                                                4180452559 3294769488
                                                1684529556 1002945951
                                                3181438866 22506664
                                                691783457 2685221343
                                                171579916 3878728600
                                                2475806724 2030324028
                                                3331164912 1708711359
                                                1970023127 2859691344
                                                2588476477 2748146879
                                                136111222 2967685492
                                                909517429 2835297809
                                                3206906216 3186870716
                                                341264097 2542035121
                                                3353277068 548223577
                                                3170936588 1678403446
                                                297435620 2337555430
                                                466603495 1132321815
                                                1208589219 696392160
                                                894244439 2562678859
                                                470224582 3306867480
                                                201364898 2075966438
                                                1767227936 2929737987
                                                3674877796 2654196643
                                                3692734598 3528895099
                                                2796780123 3048728353
                                                842329300 191554730
                                                2922459673 3489020079
                                                3979110629 1022523848
                                                2202932467 3583655201
                                                3565113719 587085778
                                                4176046313 3013713762
                                                950944241 396426791
                                                3784844662 3477431613
                                                3594592395 2782043838
                                                3392093507 3106564952
                                                2829419931 1358665591
                                                2206918825 3170783123
                                                31522386 2988194168
                                                1782249537 1105080928
                                                843500134 1225290080
                                                1521001832 3605886097
                                                2802786495 2728923319
                                                3996284304 903417639
                                                1171249804 1020374987
                                                2824535874 423621996
                                                1988534473 2493544470
                                                1008604435 1756003503
                                                1488867287 1386808992
                                                732088248 1780630732
                                                2482101014 976561178
                                                1543448953 2602866064
                                                2021139923 1952599828
                                                2360242564 2117959962
                                                2753061860 2388623612
                                                4138193781 2962920654
                                                2284970429 766920861
                                                3457264692 2879611383
                                                815055854 2332929068
                                                1254853997 3740375268
                                                3799380844 4091048725
                                                2006331129 1982546212
                                                686850534 1907447564
                                                2682801776 2780821066
                                                998290361 1342433871
                                                4195430425 607905174
                                                3902331779 2454067926
                                                1708133115 1170874362
                                                2008609376 3260320415
                                                2211196135 433538229
                                                2728786374 2189520818
                                                262554063 1182318347
                                                3710237267 1221022450
                                                715966018 2417068910
                                                2591870721 2870691989
                                                3418190842 4238214053
                                                1540704231 1575580968
                                                2095917976 4078310857
                                                2313532447 2110690783
                                                4056346629 4061784526
                                                1123218514 551538993
                                                597148360 4120175196
                                                3581618160 3181170517
                                                422862282 3227524138
                                                1713114790 662317149
                                                1230418732 928171837
                                                1324564878 1928816105
                                                1786535431 2878099422
                                                3290185549 539474248
                                                1657512683 552370646
                                                1671741683 3655312128
                                                1552739510 2605208763
                                                1441755014 181878989
                                                3124053868 1447103986
                                                3183906156 1728556020
                                                3502241336 3055466967
                                                1013272474 818402132
                                                1715099063 2900113506
                                                397254517 4194863039
                                                1009068739 232864647
                                                2540223708 2608288560
                                                2415367765 478404847
                                                3455100648 3182600021
                                                2115988978 434269567
                                                4117179324 3461774077
                                                887256537 3545801025
                                                286388911 3451742129
                                                1981164769 786667016
                                                3310123729 3097811076
                                                2224235657 2959658883
                                                3370969234 2514770915
                                                3345656436 2677010851
                                                2206236470 271648054
                                                2342188545 4292848611
                                                3646533909 3754009956
                                                3803931226 4160647125
                                                1477814055 4043852216
                                                1876372354 3133294443
                                                3871104810 3177020907
                                                2074304428 3479393793
                                                759562891 164128153
                                                1839069216 2114162633
                                                3989947309 3611054956
                                                1333547922 835429831
                                                494987340 171987910
                                                1252001001 370809172
                                                3508925425 2535703112
                                                1276855041 1922855120
                                                835673414 3030664304
                                                613287117 171219893
                                                3423096126 3376881639
                                                2287770315 1658692645
                                                1262815245 3957234326
                                                1168096164 2968737525
                                                2655813712 2132313144
                                                3976047964 326516571
                                                353088456 3679188938
                                                3205649712 2654036126
                                                1249024881 880166166
                                                691800469 2229503665
                                                1673458056 4032208375
                                                1851778863 2563757330
                                                376742205 1794655231
                                                340247333 1505873033
                                                396524441 879666767
                                                3335579166 3260764261
                                                3335999539 506221798
                                                4214658741 975887814
                                                2080536343 3360539560
                                                571586418 138896374
                                                4234352651 2737620262
                                                3928362291 1516365296
                                                38056726 3599462320
                                                3585007266 3850961033
                                                471667319 1536883193
                                                2310166751 1861637689
                                                2530999841 4139843801
                                                2710569485 827578615
                                                2012334720 2907369459
                                                3029312804 2820112398
                                                1965028045 35518606
                                                2478379033 643747771
                                                1924139484 4123405127
                                                3811735531 3429660832
                                                3285177704 1948416081
                                                1311525291 1183517742
                                                1739192232 3979815115
                                                2567840007 4116821529
                                                213304419 4125718577
                                                1473064925 2442436592
                                                1893310111 4195361916
                                                3747569474 828465101
                                                2991227658 750582866
                                                1205170309 1409813056
                                                678418130 1171531016
                                                3821236156 354504587
                                                4202874632 3882511497
                                                1893248677 1903078632
                                                26340130 2069166240
                                                3657122492 3725758099
                                                831344905 811453383
                                                3447711422 2434543565
                                                4166886888 3358210805
                                                4142984013 2988152326
                                                3527824853 982082992
                                                2809155763 190157081
                                                3340214818 2365432395
                                                2548636180 2894533366
                                                3474657421 2372634704
                                                2845748389 43024175
                                                2774226648 1987702864
                                                3186502468 453610222
                                                4204736567 1392892630
                                                2471323686 2470534280
                                                3541393095 4269885866
                                                3909911300 759132955
                                                1482612480 667715263
                                                1795580598 2337923983
                                                3390586366 581426223
                                                1515718634 476374295
                                                705213300 363062054
                                                2084697697 2407503428
                                                2292957699 2426213835
                                                2199989172 1987356470
                                                4026755612 2147252133
                                                270400031 1367820199
                                                2369854699 2844269403
                                                79981964)))

```

### `*READ-BASE*`

```lisp
COMMON-LISP:*READ-BASE*
  [symbol]

*READ-BASE* names a special variable:
  Declared type: (INTEGER 2 36)
  Declared always-bound.
  Value: 10
  Documentation:
    the radix that Lisp reads numbers in

```

### `*READ-DEFAULT-FLOAT-FORMAT*`

```lisp
COMMON-LISP:*READ-DEFAULT-FLOAT-FORMAT*
  [symbol]

*READ-DEFAULT-FLOAT-FORMAT* names a special variable:
  Declared type: (MEMBER RATIONAL LONG-FLOAT SHORT-FLOAT DOUBLE-FLOAT
                         SINGLE-FLOAT)
  Declared always-bound.
  Value: SINGLE-FLOAT

```

### `*READ-EVAL*`

```lisp
COMMON-LISP:*READ-EVAL*
  [symbol]

*READ-EVAL* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: T
  Documentation:
    If false, then the #. read macro is disabled.

```

### `*READ-SUPPRESS*`

```lisp
COMMON-LISP:*READ-SUPPRESS*
  [symbol]

*READ-SUPPRESS* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    Suppress most interpreting in the reader when T.

```

### `*READTABLE*`

```lisp
COMMON-LISP:*READTABLE*
  [symbol]

*READTABLE* names a special variable:
  Declared type: READTABLE
  Declared always-bound.
  Value: #<READTABLE {1000022413}>
  Documentation:
    Variable bound to current readtable.

```

### `*STANDARD-INPUT*`

```lisp
COMMON-LISP:*STANDARD-INPUT*
  [symbol]

*STANDARD-INPUT* names a special variable:
  Declared type: STREAM
  Declared always-bound.
  Value: #<ALIVE/SBCL/STREAMS:IO-STREAM {1003EE8143}>
  Documentation:
    default input stream

```

### `*STANDARD-OUTPUT*`

```lisp
COMMON-LISP:*STANDARD-OUTPUT*
  [symbol]

*STANDARD-OUTPUT* names a special variable:
  Declared type: STREAM
  Declared always-bound.
  Value: #<ALIVE/SBCL/STREAMS:IO-STREAM {1003EE8143}>
  Documentation:
    default output stream

```

### `*TERMINAL-IO*`

```lisp
COMMON-LISP:*TERMINAL-IO*
  [symbol]

*TERMINAL-IO* names a special variable:
  Declared type: STREAM
  Declared always-bound.
  Value: #<SYNONYM-STREAM :SYMBOL SB-SYS:*TTY* {100000DA83}>
  Documentation:
    terminal I/O stream

```

### `*TRACE-OUTPUT*`

```lisp
COMMON-LISP:*TRACE-OUTPUT*
  [symbol]

*TRACE-OUTPUT* names a special variable:
  Declared type: STREAM
  Declared always-bound.
  Value: #<ALIVE/SBCL/STREAMS:IO-STREAM {1003EE81B3}>
  Documentation:
    trace output stream

```

### `+`

```lisp
COMMON-LISP:+
  [symbol]

+ names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the value of the most recent top level READ

+ names a compiled function:
  Lambda-list: (&REST NUMBERS)
  Declared type: (FUNCTION (&REST NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Return the sum of its arguments. With no args, returns 0.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `++`

```lisp
COMMON-LISP:++
  [symbol]

++ names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the previous value of +

```

### `+++`

```lisp
COMMON-LISP:+++
  [symbol]

+++ names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the previous value of ++

```

### `-`

```lisp
COMMON-LISP:-
  [symbol]

- names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the form currently being evaluated

- names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (NUMBER &REST NUMBER)
                  (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Subtract the second and all subsequent arguments from the first;
      or with one argument, negate the first argument.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `/`

```lisp
COMMON-LISP:/
  [symbol]

/ names a special variable:
  Declared type: LIST
  Declared always-bound.
  Value: NIL
  Documentation:
    a list of all the values returned by the most recent top level EVAL

/ names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (NUMBER &REST NUMBER)
                  (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Divide the first argument by each of the following arguments, in turn.
      With one argument, return reciprocal.
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `//`

```lisp
COMMON-LISP://
  [symbol]

// names a special variable:
  Declared type: LIST
  Declared always-bound.
  Value: NIL
  Documentation:
    the previous value of /

```

### `///`

```lisp
COMMON-LISP:///
  [symbol]

/// names a special variable:
  Declared type: LIST
  Declared always-bound.
  Value: NIL
  Documentation:
    the previous value of //

```


## compiled function

### Symbol list

- [special variable](#special-variable)
  - [Symbol list](#symbol-list)
  - [`*`](#)
  - [`**`](#-1)
  - [`***`](#-2)
  - [`*BREAK-ON-SIGNALS*`](#break-on-signals)
  - [`*COMPILE-FILE-PATHNAME*`](#compile-file-pathname)
  - [`*COMPILE-FILE-TRUENAME*`](#compile-file-truename)
  - [`*COMPILE-PRINT*`](#compile-print)
  - [`*COMPILE-VERBOSE*`](#compile-verbose)
  - [`*DEBUG-IO*`](#debug-io)
  - [`*DEBUGGER-HOOK*`](#debugger-hook)
  - [`*DEFAULT-PATHNAME-DEFAULTS*`](#default-pathname-defaults)
  - [`*ERROR-OUTPUT*`](#error-output)
  - [`*FEATURES*`](#features)
  - [`*GENSYM-COUNTER*`](#gensym-counter)
  - [`*LOAD-PATHNAME*`](#load-pathname)
  - [`*LOAD-PRINT*`](#load-print)
  - [`*LOAD-TRUENAME*`](#load-truename)
  - [`*LOAD-VERBOSE*`](#load-verbose)
  - [`*MACROEXPAND-HOOK*`](#macroexpand-hook)
  - [`*MODULES*`](#modules)
  - [`*PACKAGE*`](#package)
  - [`*PRINT-ARRAY*`](#print-array)
  - [`*PRINT-BASE*`](#print-base)
  - [`*PRINT-CASE*`](#print-case)
  - [`*PRINT-CIRCLE*`](#print-circle)
  - [`*PRINT-ESCAPE*`](#print-escape)
  - [`*PRINT-GENSYM*`](#print-gensym)
  - [`*PRINT-LENGTH*`](#print-length)
  - [`*PRINT-LEVEL*`](#print-level)
  - [`*PRINT-LINES*`](#print-lines)
  - [`*PRINT-MISER-WIDTH*`](#print-miser-width)
  - [`*PRINT-PPRINT-DISPATCH*`](#print-pprint-dispatch)
  - [`*PRINT-PRETTY*`](#print-pretty)
  - [`*PRINT-RADIX*`](#print-radix)
  - [`*PRINT-READABLY*`](#print-readably)
  - [`*PRINT-RIGHT-MARGIN*`](#print-right-margin)
  - [`*QUERY-IO*`](#query-io)
  - [`*RANDOM-STATE*`](#random-state)
  - [`*READ-BASE*`](#read-base)
  - [`*READ-DEFAULT-FLOAT-FORMAT*`](#read-default-float-format)
  - [`*READ-EVAL*`](#read-eval)
  - [`*READ-SUPPRESS*`](#read-suppress)
  - [`*READTABLE*`](#readtable)
  - [`*STANDARD-INPUT*`](#standard-input)
  - [`*STANDARD-OUTPUT*`](#standard-output)
  - [`*TERMINAL-IO*`](#terminal-io)
  - [`*TRACE-OUTPUT*`](#trace-output)
  - [`+`](#-3)
  - [`++`](#-4)
  - [`+++`](#-5)
  - [`-`](#-)
  - [`/`](#-6)
  - [`//`](#-7)
  - [`///`](#-8)
- [compiled function](#compiled-function)
  - [Symbol list](#symbol-list-1)
  - [`*`](#-9)
  - [`+`](#-10)
  - [`-`](#--1)
  - [`/`](#-11)
  - [`/=`](#-12)
  - [`1+`](#1)
  - [`1-`](#1-)
  - [`<`](#-13)
  - [`<=`](#-14)
  - [`=`](#-15)
  - [`>`](#-16)
  - [`>=`](#-17)
  - [`ABORT`](#abort)
  - [`ABS`](#abs)
  - [`ACONS`](#acons)
  - [`ACOS`](#acos)
  - [`ACOSH`](#acosh)
  - [`ADJOIN`](#adjoin)
  - [`ADJUST-ARRAY`](#adjust-array)
  - [`ADJUSTABLE-ARRAY-P`](#adjustable-array-p)
  - [`ALPHA-CHAR-P`](#alpha-char-p)
  - [`ALPHANUMERICP`](#alphanumericp)
  - [`APPEND`](#append)
  - [`APPLY`](#apply)
  - [`APROPOS`](#apropos)
  - [`APROPOS-LIST`](#apropos-list)
  - [`AREF`](#aref)
  - [`(SETF AREF)`](#setf-aref)
  - [`ARITHMETIC-ERROR-OPERANDS`](#arithmetic-error-operands)
  - [`ARITHMETIC-ERROR-OPERATION`](#arithmetic-error-operation)
  - [`ARRAY-DIMENSION`](#array-dimension)
  - [`ARRAY-DIMENSIONS`](#array-dimensions)
  - [`ARRAY-DISPLACEMENT`](#array-displacement)
  - [`ARRAY-ELEMENT-TYPE`](#array-element-type)
  - [`ARRAY-HAS-FILL-POINTER-P`](#array-has-fill-pointer-p)
  - [`ARRAY-IN-BOUNDS-P`](#array-in-bounds-p)
  - [`ARRAY-RANK`](#array-rank)
  - [`ARRAY-ROW-MAJOR-INDEX`](#array-row-major-index)
  - [`ARRAY-TOTAL-SIZE`](#array-total-size)
  - [`ARRAYP`](#arrayp)
  - [`ASH`](#ash)
  - [`ASIN`](#asin)
  - [`ASINH`](#asinh)
  - [`ASSOC`](#assoc)
  - [`ASSOC-IF`](#assoc-if)
  - [`ASSOC-IF-NOT`](#assoc-if-not)
  - [`ATAN`](#atan)
  - [`ATANH`](#atanh)
  - [`ATOM`](#atom)
  - [`BIT`](#bit)
  - [`(SETF BIT)`](#setf-bit)
  - [`BIT-AND`](#bit-and)
  - [`BIT-ANDC1`](#bit-andc1)
  - [`BIT-ANDC2`](#bit-andc2)
  - [`BIT-EQV`](#bit-eqv)
  - [`BIT-IOR`](#bit-ior)
  - [`BIT-NAND`](#bit-nand)
  - [`BIT-NOR`](#bit-nor)
  - [`BIT-NOT`](#bit-not)
  - [`BIT-ORC1`](#bit-orc1)
  - [`BIT-ORC2`](#bit-orc2)
  - [`BIT-VECTOR-P`](#bit-vector-p)
  - [`BIT-XOR`](#bit-xor)
  - [`BOOLE`](#boole)
  - [`BOTH-CASE-P`](#both-case-p)
  - [`BOUNDP`](#boundp)
  - [`BREAK`](#break)
  - [`BROADCAST-STREAM-STREAMS`](#broadcast-stream-streams)
  - [`BUTLAST`](#butlast)
  - [`BYTE`](#byte)
  - [`BYTE-POSITION`](#byte-position)
  - [`BYTE-SIZE`](#byte-size)
  - [`CAAAAR`](#caaaar)
  - [`(SETF CAAAAR)`](#setf-caaaar)
  - [`CAAADR`](#caaadr)
  - [`(SETF CAAADR)`](#setf-caaadr)
  - [`CAAAR`](#caaar)
  - [`(SETF CAAAR)`](#setf-caaar)
  - [`CAADAR`](#caadar)
  - [`(SETF CAADAR)`](#setf-caadar)
  - [`CAADDR`](#caaddr)
  - [`(SETF CAADDR)`](#setf-caaddr)
  - [`CAADR`](#caadr)
  - [`(SETF CAADR)`](#setf-caadr)
  - [`CAAR`](#caar)
  - [`(SETF CAAR)`](#setf-caar)
  - [`CADAAR`](#cadaar)
  - [`(SETF CADAAR)`](#setf-cadaar)
  - [`CADADR`](#cadadr)
  - [`(SETF CADADR)`](#setf-cadadr)
  - [`CADAR`](#cadar)
  - [`(SETF CADAR)`](#setf-cadar)
  - [`CADDAR`](#caddar)
  - [`(SETF CADDAR)`](#setf-caddar)
  - [`CADDDR`](#cadddr)
  - [`(SETF CADDDR)`](#setf-cadddr)
  - [`CADDR`](#caddr)
  - [`(SETF CADDR)`](#setf-caddr)
  - [`CADR`](#cadr)
  - [`(SETF CADR)`](#setf-cadr)
  - [`CAR`](#car)
  - [`(SETF CAR)`](#setf-car)
  - [`CDAAAR`](#cdaaar)
  - [`(SETF CDAAAR)`](#setf-cdaaar)
  - [`CDAADR`](#cdaadr)
  - [`(SETF CDAADR)`](#setf-cdaadr)
  - [`CDAAR`](#cdaar)
  - [`(SETF CDAAR)`](#setf-cdaar)
  - [`CDADAR`](#cdadar)
  - [`(SETF CDADAR)`](#setf-cdadar)
  - [`CDADDR`](#cdaddr)
  - [`(SETF CDADDR)`](#setf-cdaddr)
  - [`CDADR`](#cdadr)
  - [`(SETF CDADR)`](#setf-cdadr)
  - [`CDAR`](#cdar)
  - [`(SETF CDAR)`](#setf-cdar)
  - [`CDDAAR`](#cddaar)
  - [`(SETF CDDAAR)`](#setf-cddaar)
  - [`CDDADR`](#cddadr)
  - [`(SETF CDDADR)`](#setf-cddadr)
  - [`CDDAR`](#cddar)
  - [`(SETF CDDAR)`](#setf-cddar)
  - [`CDDDAR`](#cdddar)
  - [`(SETF CDDDAR)`](#setf-cdddar)
  - [`CDDDDR`](#cddddr)
  - [`(SETF CDDDDR)`](#setf-cddddr)
  - [`CDDDR`](#cdddr)
  - [`(SETF CDDDR)`](#setf-cdddr)
  - [`CDDR`](#cddr)
  - [`(SETF CDDR)`](#setf-cddr)
  - [`CDR`](#cdr)
  - [`(SETF CDR)`](#setf-cdr)
  - [`CEILING`](#ceiling)
  - [`CELL-ERROR-NAME`](#cell-error-name)
  - [`CERROR`](#cerror)
  - [`CHAR`](#char)
  - [`(SETF CHAR)`](#setf-char)
  - [`CHAR-CODE`](#char-code)
  - [`CHAR-DOWNCASE`](#char-downcase)
  - [`CHAR-EQUAL`](#char-equal)
  - [`CHAR-GREATERP`](#char-greaterp)
  - [`CHAR-INT`](#char-int)
  - [`CHAR-LESSP`](#char-lessp)
  - [`CHAR-NAME`](#char-name)
  - [`CHAR-NOT-EQUAL`](#char-not-equal)
  - [`CHAR-NOT-GREATERP`](#char-not-greaterp)
  - [`CHAR-NOT-LESSP`](#char-not-lessp)
  - [`CHAR-UPCASE`](#char-upcase)
  - [`CHAR/=`](#char-1)
  - [`CHAR<`](#char-2)
  - [`CHAR<=`](#char-3)
  - [`CHAR=`](#char-4)
  - [`CHAR>`](#char-5)
  - [`CHAR>=`](#char-6)
  - [`CHARACTER`](#character)
  - [`CHARACTERP`](#characterp)
  - [`CIS`](#cis)
  - [`CLASS-OF`](#class-of)
  - [`CLEAR-INPUT`](#clear-input)
  - [`CLEAR-OUTPUT`](#clear-output)
  - [`CLRHASH`](#clrhash)
  - [`CODE-CHAR`](#code-char)
  - [`COERCE`](#coerce)
  - [`COMPILE`](#compile)
  - [`COMPILE-FILE`](#compile-file)
  - [`COMPILE-FILE-PATHNAME`](#compile-file-pathname-1)
  - [`COMPILED-FUNCTION-P`](#compiled-function-p)
  - [`COMPILER-MACRO-FUNCTION`](#compiler-macro-function)
  - [`(SETF COMPILER-MACRO-FUNCTION)`](#setf-compiler-macro-function)
  - [`COMPLEMENT`](#complement)
  - [`COMPLEX`](#complex)
  - [`COMPLEXP`](#complexp)
  - [`COMPUTE-RESTARTS`](#compute-restarts)
  - [`CONCATENATE`](#concatenate)
  - [`CONCATENATED-STREAM-STREAMS`](#concatenated-stream-streams)
  - [`CONJUGATE`](#conjugate)
  - [`CONS`](#cons)
  - [`CONSP`](#consp)
  - [`CONSTANTLY`](#constantly)
  - [`CONSTANTP`](#constantp)
  - [`CONTINUE`](#continue)
  - [`COPY-ALIST`](#copy-alist)
  - [`COPY-LIST`](#copy-list)
  - [`COPY-PPRINT-DISPATCH`](#copy-pprint-dispatch)
  - [`COPY-READTABLE`](#copy-readtable)
  - [`COPY-SEQ`](#copy-seq)
  - [`COPY-STRUCTURE`](#copy-structure)
  - [`COPY-SYMBOL`](#copy-symbol)
  - [`COPY-TREE`](#copy-tree)
  - [`COS`](#cos)
  - [`COSH`](#cosh)
  - [`COUNT`](#count)
  - [`COUNT-IF`](#count-if)
  - [`COUNT-IF-NOT`](#count-if-not)
  - [`DECODE-FLOAT`](#decode-float)
  - [`DECODE-UNIVERSAL-TIME`](#decode-universal-time)
  - [`DELETE`](#delete)
  - [`DELETE-DUPLICATES`](#delete-duplicates)
  - [`DELETE-FILE`](#delete-file)
  - [`DELETE-IF`](#delete-if)
  - [`DELETE-IF-NOT`](#delete-if-not)
  - [`DELETE-PACKAGE`](#delete-package)
  - [`DENOMINATOR`](#denominator)
  - [`DEPOSIT-FIELD`](#deposit-field)
  - [`DESCRIBE`](#describe)
  - [`DIGIT-CHAR`](#digit-char)
  - [`DIGIT-CHAR-P`](#digit-char-p)
  - [`DIRECTORY`](#directory)
  - [`DIRECTORY-NAMESTRING`](#directory-namestring)
  - [`DISASSEMBLE`](#disassemble)
  - [`DPB`](#dpb)
  - [`DRIBBLE`](#dribble)
  - [`ECHO-STREAM-INPUT-STREAM`](#echo-stream-input-stream)
  - [`ECHO-STREAM-OUTPUT-STREAM`](#echo-stream-output-stream)
  - [`ED`](#ed)
  - [`EIGHTH`](#eighth)
  - [`(SETF EIGHTH)`](#setf-eighth)
  - [`ELT`](#elt)
  - [`(SETF ELT)`](#setf-elt)
  - [`ENCODE-UNIVERSAL-TIME`](#encode-universal-time)
  - [`ENDP`](#endp)
  - [`ENOUGH-NAMESTRING`](#enough-namestring)
  - [`ENSURE-DIRECTORIES-EXIST`](#ensure-directories-exist)
  - [`ENSURE-GENERIC-FUNCTION`](#ensure-generic-function)
  - [`EQ`](#eq)
  - [`EQL`](#eql)
  - [`EQUAL`](#equal)
  - [`EQUALP`](#equalp)
  - [`ERROR`](#error)
  - [`EVAL`](#eval)
  - [`EVENP`](#evenp)
  - [`EVERY`](#every)
  - [`EXP`](#exp)
  - [`EXPORT`](#export)
  - [`EXPT`](#expt)
  - [`FBOUNDP`](#fboundp)
  - [`FCEILING`](#fceiling)
  - [`FDEFINITION`](#fdefinition)
  - [`(SETF FDEFINITION)`](#setf-fdefinition)
  - [`FFLOOR`](#ffloor)
  - [`FIFTH`](#fifth)
  - [`(SETF FIFTH)`](#setf-fifth)
  - [`FILE-AUTHOR`](#file-author)
  - [`FILE-ERROR-PATHNAME`](#file-error-pathname)
  - [`FILE-LENGTH`](#file-length)
  - [`FILE-NAMESTRING`](#file-namestring)
  - [`FILE-POSITION`](#file-position)
  - [`FILE-STRING-LENGTH`](#file-string-length)
  - [`FILE-WRITE-DATE`](#file-write-date)
  - [`FILL`](#fill)
  - [`FILL-POINTER`](#fill-pointer)
  - [`(SETF FILL-POINTER)`](#setf-fill-pointer)
  - [`FIND`](#find)
  - [`FIND-ALL-SYMBOLS`](#find-all-symbols)
  - [`FIND-CLASS`](#find-class)
  - [`(SETF FIND-CLASS)`](#setf-find-class)
  - [`FIND-IF`](#find-if)
  - [`FIND-IF-NOT`](#find-if-not)
  - [`FIND-PACKAGE`](#find-package)
  - [`FIND-RESTART`](#find-restart)
  - [`FIND-SYMBOL`](#find-symbol)
  - [`FINISH-OUTPUT`](#finish-output)
  - [`FIRST`](#first)
  - [`(SETF FIRST)`](#setf-first)
  - [`FLOAT`](#float)
  - [`FLOAT-DIGITS`](#float-digits)
  - [`FLOAT-PRECISION`](#float-precision)
  - [`FLOAT-RADIX`](#float-radix)
  - [`FLOAT-SIGN`](#float-sign)
  - [`FLOATP`](#floatp)
  - [`FLOOR`](#floor)
  - [`FMAKUNBOUND`](#fmakunbound)
  - [`FORCE-OUTPUT`](#force-output)
  - [`FORMAT`](#format)
  - [`FOURTH`](#fourth)
  - [`(SETF FOURTH)`](#setf-fourth)
  - [`FRESH-LINE`](#fresh-line)
  - [`FROUND`](#fround)
  - [`FTRUNCATE`](#ftruncate)
  - [`FUNCALL`](#funcall)
  - [`FUNCTION-LAMBDA-EXPRESSION`](#function-lambda-expression)
  - [`FUNCTIONP`](#functionp)
  - [`GCD`](#gcd)
  - [`GENSYM`](#gensym)
  - [`GENTEMP`](#gentemp)
  - [`GET`](#get)
  - [`GET-DECODED-TIME`](#get-decoded-time)
  - [`GET-DISPATCH-MACRO-CHARACTER`](#get-dispatch-macro-character)
  - [`GET-INTERNAL-REAL-TIME`](#get-internal-real-time)
  - [`GET-INTERNAL-RUN-TIME`](#get-internal-run-time)
  - [`GET-MACRO-CHARACTER`](#get-macro-character)
  - [`GET-OUTPUT-STREAM-STRING`](#get-output-stream-string)
  - [`GET-PROPERTIES`](#get-properties)
  - [`GET-SETF-EXPANSION`](#get-setf-expansion)
  - [`GET-UNIVERSAL-TIME`](#get-universal-time)
  - [`GETF`](#getf)
  - [`GETHASH`](#gethash)
  - [`(SETF GETHASH)`](#setf-gethash)
  - [`GRAPHIC-CHAR-P`](#graphic-char-p)
  - [`HASH-TABLE-COUNT`](#hash-table-count)
  - [`HASH-TABLE-P`](#hash-table-p)
  - [`HASH-TABLE-REHASH-SIZE`](#hash-table-rehash-size)
  - [`HASH-TABLE-REHASH-THRESHOLD`](#hash-table-rehash-threshold)
  - [`HASH-TABLE-SIZE`](#hash-table-size)
  - [`HASH-TABLE-TEST`](#hash-table-test)
  - [`HOST-NAMESTRING`](#host-namestring)
  - [`IDENTITY`](#identity)
  - [`IMAGPART`](#imagpart)
  - [`IMPORT`](#import)
  - [`INSPECT`](#inspect)
  - [`INTEGER-DECODE-FLOAT`](#integer-decode-float)
  - [`INTEGER-LENGTH`](#integer-length)
  - [`INTEGERP`](#integerp)
  - [`INTERN`](#intern)
  - [`INTERSECTION`](#intersection)
  - [`INVALID-METHOD-ERROR`](#invalid-method-error)
  - [`INVOKE-DEBUGGER`](#invoke-debugger)
  - [`INVOKE-RESTART`](#invoke-restart)
  - [`INVOKE-RESTART-INTERACTIVELY`](#invoke-restart-interactively)
  - [`ISQRT`](#isqrt)
  - [`KEYWORDP`](#keywordp)
  - [`LAST`](#last)
  - [`LCM`](#lcm)
  - [`LDB`](#ldb)
  - [`LDB-TEST`](#ldb-test)
  - [`LDIFF`](#ldiff)
  - [`LENGTH`](#length)
  - [`LISP-IMPLEMENTATION-TYPE`](#lisp-implementation-type)
  - [`LISP-IMPLEMENTATION-VERSION`](#lisp-implementation-version)
  - [`LIST`](#list)
  - [`LIST*`](#list-1)
  - [`LIST-ALL-PACKAGES`](#list-all-packages)
  - [`LIST-LENGTH`](#list-length)
  - [`LISTEN`](#listen)
  - [`LISTP`](#listp)
  - [`LOAD`](#load)
  - [`LOAD-LOGICAL-PATHNAME-TRANSLATIONS`](#load-logical-pathname-translations)
  - [`LOG`](#log)
  - [`LOGAND`](#logand)
  - [`LOGANDC1`](#logandc1)
  - [`LOGANDC2`](#logandc2)
  - [`LOGBITP`](#logbitp)
  - [`LOGCOUNT`](#logcount)
  - [`LOGEQV`](#logeqv)
  - [`LOGICAL-PATHNAME`](#logical-pathname)
  - [`LOGICAL-PATHNAME-TRANSLATIONS`](#logical-pathname-translations)
  - [`(SETF LOGICAL-PATHNAME-TRANSLATIONS)`](#setf-logical-pathname-translations)
  - [`LOGIOR`](#logior)
  - [`LOGNAND`](#lognand)
  - [`LOGNOR`](#lognor)
  - [`LOGNOT`](#lognot)
  - [`LOGORC1`](#logorc1)
  - [`LOGORC2`](#logorc2)
  - [`LOGTEST`](#logtest)
  - [`LOGXOR`](#logxor)
  - [`LONG-SITE-NAME`](#long-site-name)
  - [`LOWER-CASE-P`](#lower-case-p)
  - [`MACHINE-INSTANCE`](#machine-instance)
  - [`MACHINE-TYPE`](#machine-type)
  - [`MACHINE-VERSION`](#machine-version)
  - [`MACRO-FUNCTION`](#macro-function)
  - [`(SETF MACRO-FUNCTION)`](#setf-macro-function)
  - [`MACROEXPAND`](#macroexpand)
  - [`MACROEXPAND-1`](#macroexpand-1)
  - [`MAKE-ARRAY`](#make-array)
  - [`MAKE-BROADCAST-STREAM`](#make-broadcast-stream)
  - [`MAKE-CONCATENATED-STREAM`](#make-concatenated-stream)
  - [`MAKE-CONDITION`](#make-condition)
  - [`MAKE-DISPATCH-MACRO-CHARACTER`](#make-dispatch-macro-character)
  - [`MAKE-ECHO-STREAM`](#make-echo-stream)
  - [`MAKE-HASH-TABLE`](#make-hash-table)
  - [`MAKE-LIST`](#make-list)
  - [`MAKE-LOAD-FORM-SAVING-SLOTS`](#make-load-form-saving-slots)
  - [`MAKE-PACKAGE`](#make-package)
  - [`MAKE-PATHNAME`](#make-pathname)
  - [`MAKE-RANDOM-STATE`](#make-random-state)
  - [`MAKE-SEQUENCE`](#make-sequence)
  - [`MAKE-STRING`](#make-string)
  - [`MAKE-STRING-INPUT-STREAM`](#make-string-input-stream)
  - [`MAKE-STRING-OUTPUT-STREAM`](#make-string-output-stream)
  - [`MAKE-SYMBOL`](#make-symbol)
  - [`MAKE-SYNONYM-STREAM`](#make-synonym-stream)
  - [`MAKE-TWO-WAY-STREAM`](#make-two-way-stream)
  - [`MAKUNBOUND`](#makunbound)
  - [`MAP`](#map)
  - [`MAP-INTO`](#map-into)
  - [`MAPC`](#mapc)
  - [`MAPCAN`](#mapcan)
  - [`MAPCAR`](#mapcar)
  - [`MAPCON`](#mapcon)
  - [`MAPHASH`](#maphash)
  - [`MAPL`](#mapl)
  - [`MAPLIST`](#maplist)
  - [`MASK-FIELD`](#mask-field)
  - [`MAX`](#max)
  - [`MEMBER`](#member)
  - [`MEMBER-IF`](#member-if)
  - [`MEMBER-IF-NOT`](#member-if-not)
  - [`MERGE`](#merge)
  - [`MERGE-PATHNAMES`](#merge-pathnames)
  - [`METHOD-COMBINATION-ERROR`](#method-combination-error)
  - [`MIN`](#min)
  - [`MINUSP`](#minusp)
  - [`MISMATCH`](#mismatch)
  - [`MOD`](#mod)
  - [`MUFFLE-WARNING`](#muffle-warning)
  - [`NAME-CHAR`](#name-char)
  - [`NAMESTRING`](#namestring)
  - [`NBUTLAST`](#nbutlast)
  - [`NCONC`](#nconc)
  - [`NINTERSECTION`](#nintersection)
  - [`NINTH`](#ninth)
  - [`(SETF NINTH)`](#setf-ninth)
  - [`NOT`](#not)
  - [`NOTANY`](#notany)
  - [`NOTEVERY`](#notevery)
  - [`NRECONC`](#nreconc)
  - [`NREVERSE`](#nreverse)
  - [`NSET-DIFFERENCE`](#nset-difference)
  - [`NSET-EXCLUSIVE-OR`](#nset-exclusive-or)
  - [`NSTRING-CAPITALIZE`](#nstring-capitalize)
  - [`NSTRING-DOWNCASE`](#nstring-downcase)
  - [`NSTRING-UPCASE`](#nstring-upcase)
  - [`NSUBLIS`](#nsublis)
  - [`NSUBST`](#nsubst)
  - [`NSUBST-IF`](#nsubst-if)
  - [`NSUBST-IF-NOT`](#nsubst-if-not)
  - [`NSUBSTITUTE`](#nsubstitute)
  - [`NSUBSTITUTE-IF`](#nsubstitute-if)
  - [`NSUBSTITUTE-IF-NOT`](#nsubstitute-if-not)
  - [`NTH`](#nth)
  - [`(SETF NTH)`](#setf-nth)
  - [`NTHCDR`](#nthcdr)
  - [`NULL`](#null)
  - [`NUMBERP`](#numberp)
  - [`NUMERATOR`](#numerator)
  - [`NUNION`](#nunion)
  - [`ODDP`](#oddp)
  - [`OPEN`](#open)
  - [`PACKAGE-ERROR-PACKAGE`](#package-error-package)
  - [`PACKAGE-NAME`](#package-name)
  - [`PACKAGE-NICKNAMES`](#package-nicknames)
  - [`PACKAGE-SHADOWING-SYMBOLS`](#package-shadowing-symbols)
  - [`PACKAGE-USE-LIST`](#package-use-list)
  - [`PACKAGE-USED-BY-LIST`](#package-used-by-list)
  - [`PACKAGEP`](#packagep)
  - [`PAIRLIS`](#pairlis)
  - [`PARSE-INTEGER`](#parse-integer)
  - [`PARSE-NAMESTRING`](#parse-namestring)
  - [`PATHNAME`](#pathname)
  - [`PATHNAME-DEVICE`](#pathname-device)
  - [`PATHNAME-DIRECTORY`](#pathname-directory)
  - [`PATHNAME-HOST`](#pathname-host)
  - [`PATHNAME-MATCH-P`](#pathname-match-p)
  - [`PATHNAME-NAME`](#pathname-name)
  - [`PATHNAME-TYPE`](#pathname-type)
  - [`PATHNAME-VERSION`](#pathname-version)
  - [`PATHNAMEP`](#pathnamep)
  - [`PEEK-CHAR`](#peek-char)
  - [`PHASE`](#phase)
  - [`PLUSP`](#plusp)
  - [`POSITION`](#position)
  - [`POSITION-IF`](#position-if)
  - [`POSITION-IF-NOT`](#position-if-not)
  - [`PPRINT`](#pprint)
  - [`PPRINT-DISPATCH`](#pprint-dispatch)
  - [`PPRINT-FILL`](#pprint-fill)
  - [`PPRINT-INDENT`](#pprint-indent)
  - [`PPRINT-LINEAR`](#pprint-linear)
  - [`PPRINT-NEWLINE`](#pprint-newline)
  - [`PPRINT-TAB`](#pprint-tab)
  - [`PPRINT-TABULAR`](#pprint-tabular)
  - [`PRIN1`](#prin1)
  - [`PRIN1-TO-STRING`](#prin1-to-string)
  - [`PRINC`](#princ)
  - [`PRINC-TO-STRING`](#princ-to-string)
  - [`PRINT`](#print)
  - [`PRINT-NOT-READABLE-OBJECT`](#print-not-readable-object)
  - [`PROBE-FILE`](#probe-file)
  - [`PROCLAIM`](#proclaim)
  - [`PROVIDE`](#provide)
  - [`RANDOM`](#random)
  - [`RANDOM-STATE-P`](#random-state-p)
  - [`RASSOC`](#rassoc)
  - [`RASSOC-IF`](#rassoc-if)
  - [`RASSOC-IF-NOT`](#rassoc-if-not)
  - [`RATIONAL`](#rational)
  - [`RATIONALIZE`](#rationalize)
  - [`RATIONALP`](#rationalp)
  - [`READ`](#read)
  - [`READ-BYTE`](#read-byte)
  - [`READ-CHAR`](#read-char)
  - [`READ-CHAR-NO-HANG`](#read-char-no-hang)
  - [`READ-DELIMITED-LIST`](#read-delimited-list)
  - [`READ-FROM-STRING`](#read-from-string)
  - [`READ-LINE`](#read-line)
  - [`READ-PRESERVING-WHITESPACE`](#read-preserving-whitespace)
  - [`READ-SEQUENCE`](#read-sequence)
  - [`READTABLE-CASE`](#readtable-case)
  - [`(SETF READTABLE-CASE)`](#setf-readtable-case)
  - [`READTABLEP`](#readtablep)
  - [`REALP`](#realp)
  - [`REALPART`](#realpart)
  - [`REDUCE`](#reduce)
  - [`REM`](#rem)
  - [`REMHASH`](#remhash)
  - [`REMOVE`](#remove)
  - [`REMOVE-DUPLICATES`](#remove-duplicates)
  - [`REMOVE-IF`](#remove-if)
  - [`REMOVE-IF-NOT`](#remove-if-not)
  - [`REMPROP`](#remprop)
  - [`RENAME-FILE`](#rename-file)
  - [`RENAME-PACKAGE`](#rename-package)
  - [`REPLACE`](#replace)
  - [`REQUIRE`](#require)
  - [`REST`](#rest)
  - [`(SETF REST)`](#setf-rest)
  - [`RESTART-NAME`](#restart-name)
  - [`REVAPPEND`](#revappend)
  - [`REVERSE`](#reverse)
  - [`ROOM`](#room)
  - [`ROUND`](#round)
  - [`ROW-MAJOR-AREF`](#row-major-aref)
  - [`(SETF ROW-MAJOR-AREF)`](#setf-row-major-aref)
  - [`RPLACA`](#rplaca)
  - [`RPLACD`](#rplacd)
  - [`SBIT`](#sbit)
  - [`(SETF SBIT)`](#setf-sbit)
  - [`SCALE-FLOAT`](#scale-float)
  - [`SCHAR`](#schar)
  - [`(SETF SCHAR)`](#setf-schar)
  - [`SEARCH`](#search)
  - [`SECOND`](#second)
  - [`(SETF SECOND)`](#setf-second)
  - [`SET`](#set)
  - [`SET-DIFFERENCE`](#set-difference)
  - [`SET-DISPATCH-MACRO-CHARACTER`](#set-dispatch-macro-character)
  - [`SET-EXCLUSIVE-OR`](#set-exclusive-or)
  - [`SET-MACRO-CHARACTER`](#set-macro-character)
  - [`SET-PPRINT-DISPATCH`](#set-pprint-dispatch)
  - [`SET-SYNTAX-FROM-CHAR`](#set-syntax-from-char)
  - [`SEVENTH`](#seventh)
  - [`(SETF SEVENTH)`](#setf-seventh)
  - [`SHADOW`](#shadow)
  - [`SHADOWING-IMPORT`](#shadowing-import)
  - [`SHORT-SITE-NAME`](#short-site-name)
  - [`SIGNAL`](#signal)
  - [`SIGNUM`](#signum)
  - [`SIMPLE-BIT-VECTOR-P`](#simple-bit-vector-p)
  - [`SIMPLE-CONDITION-FORMAT-ARGUMENTS`](#simple-condition-format-arguments)
  - [`SIMPLE-CONDITION-FORMAT-CONTROL`](#simple-condition-format-control)
  - [`SIMPLE-STRING-P`](#simple-string-p)
  - [`SIMPLE-VECTOR-P`](#simple-vector-p)
  - [`SIN`](#sin)
  - [`SINH`](#sinh)
  - [`SIXTH`](#sixth)
  - [`(SETF SIXTH)`](#setf-sixth)
  - [`SLEEP`](#sleep)
  - [`SLOT-BOUNDP`](#slot-boundp)
  - [`SLOT-EXISTS-P`](#slot-exists-p)
  - [`SLOT-MAKUNBOUND`](#slot-makunbound)
  - [`SLOT-VALUE`](#slot-value)
  - [`(SETF SLOT-VALUE)`](#setf-slot-value)
  - [`SOFTWARE-TYPE`](#software-type)
  - [`SOFTWARE-VERSION`](#software-version)
  - [`SOME`](#some)
  - [`SORT`](#sort)
  - [`SPECIAL-OPERATOR-P`](#special-operator-p)
  - [`SQRT`](#sqrt)
  - [`STABLE-SORT`](#stable-sort)
  - [`STANDARD-CHAR-P`](#standard-char-p)
  - [`STORE-VALUE`](#store-value)
  - [`STREAM-ERROR-STREAM`](#stream-error-stream)
  - [`STREAM-EXTERNAL-FORMAT`](#stream-external-format)
  - [`STREAMP`](#streamp)
  - [`STRING`](#string)
  - [`STRING-CAPITALIZE`](#string-capitalize)
  - [`STRING-DOWNCASE`](#string-downcase)
  - [`STRING-EQUAL`](#string-equal)
  - [`STRING-GREATERP`](#string-greaterp)
  - [`STRING-LEFT-TRIM`](#string-left-trim)
  - [`STRING-LESSP`](#string-lessp)
  - [`STRING-NOT-EQUAL`](#string-not-equal)
  - [`STRING-NOT-GREATERP`](#string-not-greaterp)
  - [`STRING-NOT-LESSP`](#string-not-lessp)
  - [`STRING-RIGHT-TRIM`](#string-right-trim)
  - [`STRING-TRIM`](#string-trim)
  - [`STRING-UPCASE`](#string-upcase)
  - [`STRING/=`](#string-1)
  - [`STRING<`](#string-2)
  - [`STRING<=`](#string-3)
  - [`STRING=`](#string-4)
  - [`STRING>`](#string-5)
  - [`STRING>=`](#string-6)
  - [`STRINGP`](#stringp)
  - [`SUBLIS`](#sublis)
  - [`SUBSEQ`](#subseq)
  - [`SUBSETP`](#subsetp)
  - [`SUBST`](#subst)
  - [`SUBST-IF`](#subst-if)
  - [`SUBST-IF-NOT`](#subst-if-not)
  - [`SUBSTITUTE`](#substitute)
  - [`SUBSTITUTE-IF`](#substitute-if)
  - [`SUBSTITUTE-IF-NOT`](#substitute-if-not)
  - [`SUBTYPEP`](#subtypep)
  - [`SVREF`](#svref)
  - [`(SETF SVREF)`](#setf-svref)
  - [`SXHASH`](#sxhash)
  - [`SYMBOL-FUNCTION`](#symbol-function)
  - [`(SETF SYMBOL-FUNCTION)`](#setf-symbol-function)
  - [`SYMBOL-NAME`](#symbol-name)
  - [`SYMBOL-PACKAGE`](#symbol-package)
  - [`SYMBOL-PLIST`](#symbol-plist)
  - [`(SETF SYMBOL-PLIST)`](#setf-symbol-plist)
  - [`SYMBOL-VALUE`](#symbol-value)
  - [`(SETF SYMBOL-VALUE)`](#setf-symbol-value)
  - [`SYMBOLP`](#symbolp)
  - [`SYNONYM-STREAM-SYMBOL`](#synonym-stream-symbol)
  - [`TAILP`](#tailp)
  - [`TAN`](#tan)
  - [`TANH`](#tanh)
  - [`TENTH`](#tenth)
  - [`(SETF TENTH)`](#setf-tenth)
  - [`TERPRI`](#terpri)
  - [`THIRD`](#third)
  - [`(SETF THIRD)`](#setf-third)
  - [`TRANSLATE-LOGICAL-PATHNAME`](#translate-logical-pathname)
  - [`TRANSLATE-PATHNAME`](#translate-pathname)
  - [`TREE-EQUAL`](#tree-equal)
  - [`TRUENAME`](#truename)
  - [`TRUNCATE`](#truncate)
  - [`TWO-WAY-STREAM-INPUT-STREAM`](#two-way-stream-input-stream)
  - [`TWO-WAY-STREAM-OUTPUT-STREAM`](#two-way-stream-output-stream)
  - [`TYPE-ERROR-DATUM`](#type-error-datum)
  - [`TYPE-ERROR-EXPECTED-TYPE`](#type-error-expected-type)
  - [`TYPE-OF`](#type-of)
  - [`TYPEP`](#typep)
  - [`UNBOUND-SLOT-INSTANCE`](#unbound-slot-instance)
  - [`UNEXPORT`](#unexport)
  - [`UNINTERN`](#unintern)
  - [`UNION`](#union)
  - [`UNREAD-CHAR`](#unread-char)
  - [`UNUSE-PACKAGE`](#unuse-package)
  - [`UPGRADED-ARRAY-ELEMENT-TYPE`](#upgraded-array-element-type)
  - [`UPGRADED-COMPLEX-PART-TYPE`](#upgraded-complex-part-type)
  - [`UPPER-CASE-P`](#upper-case-p)
  - [`USE-PACKAGE`](#use-package)
  - [`USE-VALUE`](#use-value)
  - [`USER-HOMEDIR-PATHNAME`](#user-homedir-pathname)
  - [`VALUES`](#values)
  - [`VALUES-LIST`](#values-list)
  - [`VECTOR`](#vector)
  - [`VECTOR-POP`](#vector-pop)
  - [`VECTOR-PUSH`](#vector-push)
  - [`VECTOR-PUSH-EXTEND`](#vector-push-extend)
  - [`VECTORP`](#vectorp)
  - [`WARN`](#warn)
  - [`WILD-PATHNAME-P`](#wild-pathname-p)
  - [`WRITE`](#write)
  - [`WRITE-BYTE`](#write-byte)
  - [`WRITE-CHAR`](#write-char)
  - [`WRITE-LINE`](#write-line)
  - [`WRITE-SEQUENCE`](#write-sequence)
  - [`WRITE-STRING`](#write-string)
  - [`WRITE-TO-STRING`](#write-to-string)
  - [`Y-OR-N-P`](#y-or-n-p)
  - [`YES-OR-NO-P`](#yes-or-no-p)
  - [`ZEROP`](#zerop)
- [generic function](#generic-function)
  - [Symbol list](#symbol-list-2)
  - [`ADD-METHOD`](#add-method)
  - [`ALLOCATE-INSTANCE`](#allocate-instance)
  - [`CHANGE-CLASS`](#change-class)
  - [`CLASS-NAME`](#class-name)
  - [`(SETF CLASS-NAME)`](#setf-class-name)
  - [`CLOSE`](#close)
  - [`COMPUTE-APPLICABLE-METHODS`](#compute-applicable-methods)
  - [`DESCRIBE-OBJECT`](#describe-object)
  - [`DOCUMENTATION`](#documentation)
  - [`(SETF DOCUMENTATION)`](#setf-documentation)
  - [`FIND-METHOD`](#find-method)
  - [`FUNCTION-KEYWORDS`](#function-keywords)
  - [`INITIALIZE-INSTANCE`](#initialize-instance)
  - [`INPUT-STREAM-P`](#input-stream-p)
  - [`INTERACTIVE-STREAM-P`](#interactive-stream-p)
  - [`MAKE-INSTANCE`](#make-instance)
  - [`MAKE-INSTANCES-OBSOLETE`](#make-instances-obsolete)
  - [`MAKE-LOAD-FORM`](#make-load-form)
  - [`METHOD-QUALIFIERS`](#method-qualifiers)
  - [`NO-APPLICABLE-METHOD`](#no-applicable-method)
  - [`NO-NEXT-METHOD`](#no-next-method)
  - [`OPEN-STREAM-P`](#open-stream-p)
  - [`OUTPUT-STREAM-P`](#output-stream-p)
  - [`PRINT-OBJECT`](#print-object)
  - [`REINITIALIZE-INSTANCE`](#reinitialize-instance)
  - [`REMOVE-METHOD`](#remove-method)
  - [`SHARED-INITIALIZE`](#shared-initialize)
  - [`SLOT-MISSING`](#slot-missing)
  - [`SLOT-UNBOUND`](#slot-unbound)
  - [`STREAM-ELEMENT-TYPE`](#stream-element-type)
  - [`UPDATE-INSTANCE-FOR-DIFFERENT-CLASS`](#update-instance-for-different-class)
  - [`UPDATE-INSTANCE-FOR-REDEFINED-CLASS`](#update-instance-for-redefined-class)
- [macro](#macro)
  - [Symbol list](#symbol-list-3)
  - [`AND`](#and)
  - [`ASSERT`](#assert)
  - [`CALL-METHOD`](#call-method)
  - [`CASE`](#case)
  - [`CCASE`](#ccase)
  - [`CHECK-TYPE`](#check-type)
  - [`COND`](#cond)
  - [`CTYPECASE`](#ctypecase)
  - [`DECF`](#decf)
  - [`DECLAIM`](#declaim)
  - [`DEFCLASS`](#defclass)
  - [`DEFCONSTANT`](#defconstant)
  - [`DEFGENERIC`](#defgeneric)
  - [`DEFINE-COMPILER-MACRO`](#define-compiler-macro)
  - [`DEFINE-CONDITION`](#define-condition)
  - [`DEFINE-METHOD-COMBINATION`](#define-method-combination)
  - [`DEFINE-MODIFY-MACRO`](#define-modify-macro)
  - [`DEFINE-SETF-EXPANDER`](#define-setf-expander)
  - [`DEFINE-SYMBOL-MACRO`](#define-symbol-macro)
  - [`DEFMACRO`](#defmacro)
  - [`DEFMETHOD`](#defmethod)
  - [`DEFPACKAGE`](#defpackage)
  - [`DEFPARAMETER`](#defparameter)
  - [`DEFSETF`](#defsetf)
  - [`DEFSTRUCT`](#defstruct)
  - [`DEFTYPE`](#deftype)
  - [`DEFUN`](#defun)
  - [`DEFVAR`](#defvar)
  - [`DESTRUCTURING-BIND`](#destructuring-bind)
  - [`DO`](#do)
  - [`DO*`](#do-1)
  - [`DO-ALL-SYMBOLS`](#do-all-symbols)
  - [`DO-EXTERNAL-SYMBOLS`](#do-external-symbols)
  - [`DO-SYMBOLS`](#do-symbols)
  - [`DOLIST`](#dolist)
  - [`DOTIMES`](#dotimes)
  - [`ECASE`](#ecase)
  - [`ETYPECASE`](#etypecase)
  - [`FORMATTER`](#formatter)
  - [`HANDLER-BIND`](#handler-bind)
  - [`HANDLER-CASE`](#handler-case)
  - [`IGNORE-ERRORS`](#ignore-errors)
  - [`IN-PACKAGE`](#in-package)
  - [`INCF`](#incf)
  - [`LAMBDA`](#lambda)
  - [`LOOP`](#loop)
  - [`LOOP-FINISH`](#loop-finish)
  - [`MULTIPLE-VALUE-BIND`](#multiple-value-bind)
  - [`MULTIPLE-VALUE-LIST`](#multiple-value-list)
  - [`MULTIPLE-VALUE-SETQ`](#multiple-value-setq)
  - [`NTH-VALUE`](#nth-value)
  - [`OR`](#or)
  - [`POP`](#pop)
  - [`PPRINT-EXIT-IF-LIST-EXHAUSTED`](#pprint-exit-if-list-exhausted)
  - [`PPRINT-LOGICAL-BLOCK`](#pprint-logical-block)
  - [`PPRINT-POP`](#pprint-pop)
  - [`PRINT-UNREADABLE-OBJECT`](#print-unreadable-object)
  - [`PROG`](#prog)
  - [`PROG*`](#prog-1)
  - [`PROG1`](#prog1)
  - [`PROG2`](#prog2)
  - [`PSETF`](#psetf)
  - [`PSETQ`](#psetq)
  - [`PUSH`](#push)
  - [`PUSHNEW`](#pushnew)
  - [`REMF`](#remf)
  - [`RESTART-BIND`](#restart-bind)
  - [`RESTART-CASE`](#restart-case)
  - [`RETURN`](#return)
  - [`ROTATEF`](#rotatef)
  - [`SETF`](#setf)
  - [`SHIFTF`](#shiftf)
  - [`STEP`](#step)
  - [`TIME`](#time)
  - [`TRACE`](#trace)
  - [`TYPECASE`](#typecase)
  - [`UNLESS`](#unless)
  - [`UNTRACE`](#untrace)
  - [`WHEN`](#when)
  - [`WITH-ACCESSORS`](#with-accessors)
  - [`WITH-COMPILATION-UNIT`](#with-compilation-unit)
  - [`WITH-CONDITION-RESTARTS`](#with-condition-restarts)
  - [`WITH-HASH-TABLE-ITERATOR`](#with-hash-table-iterator)
  - [`WITH-INPUT-FROM-STRING`](#with-input-from-string)
  - [`WITH-OPEN-FILE`](#with-open-file)
  - [`WITH-OPEN-STREAM`](#with-open-stream)
  - [`WITH-OUTPUT-TO-STRING`](#with-output-to-string)
  - [`WITH-PACKAGE-ITERATOR`](#with-package-iterator)
  - [`WITH-SIMPLE-RESTART`](#with-simple-restart)
  - [`WITH-SLOTS`](#with-slots)
  - [`WITH-STANDARD-IO-SYNTAX`](#with-standard-io-syntax)
- [primitive type-specifier](#primitive-type-specifier)
  - [Symbol list](#symbol-list-4)
  - [`ARRAY`](#array)
  - [`ATOM`](#atom-1)
  - [`BASE-CHAR`](#base-char)
  - [`BASE-STRING`](#base-string)
  - [`BIT`](#bit-1)
  - [`BIT-VECTOR`](#bit-vector)
  - [`COMPILED-FUNCTION`](#compiled-function-1)
  - [`COMPLEX`](#complex-1)
  - [`CONS`](#cons-1)
  - [`DOUBLE-FLOAT`](#double-float)
  - [`EXTENDED-CHAR`](#extended-char)
  - [`FLOAT`](#float-1)
  - [`FUNCTION`](#function)
  - [`INTEGER`](#integer)
  - [`KEYWORD`](#keyword)
  - [`LONG-FLOAT`](#long-float)
  - [`RATIONAL`](#rational-1)
  - [`REAL`](#real)
  - [`SHORT-FLOAT`](#short-float)
  - [`SIMPLE-ARRAY`](#simple-array)
  - [`SIMPLE-BASE-STRING`](#simple-base-string)
  - [`SIMPLE-BIT-VECTOR`](#simple-bit-vector)
  - [`SIMPLE-STRING`](#simple-string)
  - [`SIMPLE-VECTOR`](#simple-vector)
  - [`SINGLE-FLOAT`](#single-float)
  - [`STANDARD-CHAR`](#standard-char)
  - [`STRING`](#string-7)
  - [`VECTOR`](#vector-1)
- [constant variable](#constant-variable)
  - [Symbol list](#symbol-list-5)
  - [`ARRAY-DIMENSION-LIMIT`](#array-dimension-limit)
  - [`ARRAY-RANK-LIMIT`](#array-rank-limit)
  - [`ARRAY-TOTAL-SIZE-LIMIT`](#array-total-size-limit)
  - [`BOOLE-1`](#boole-1)
  - [`BOOLE-2`](#boole-2)
  - [`BOOLE-AND`](#boole-and)
  - [`BOOLE-ANDC1`](#boole-andc1)
  - [`BOOLE-ANDC2`](#boole-andc2)
  - [`BOOLE-C1`](#boole-c1)
  - [`BOOLE-C2`](#boole-c2)
  - [`BOOLE-CLR`](#boole-clr)
  - [`BOOLE-EQV`](#boole-eqv)
  - [`BOOLE-IOR`](#boole-ior)
  - [`BOOLE-NAND`](#boole-nand)
  - [`BOOLE-NOR`](#boole-nor)
  - [`BOOLE-ORC1`](#boole-orc1)
  - [`BOOLE-ORC2`](#boole-orc2)
  - [`BOOLE-SET`](#boole-set)
  - [`BOOLE-XOR`](#boole-xor)
  - [`CALL-ARGUMENTS-LIMIT`](#call-arguments-limit)
  - [`CHAR-CODE-LIMIT`](#char-code-limit)
  - [`DOUBLE-FLOAT-EPSILON`](#double-float-epsilon)
  - [`DOUBLE-FLOAT-NEGATIVE-EPSILON`](#double-float-negative-epsilon)
  - [`INTERNAL-TIME-UNITS-PER-SECOND`](#internal-time-units-per-second)
  - [`LAMBDA-LIST-KEYWORDS`](#lambda-list-keywords)
  - [`LAMBDA-PARAMETERS-LIMIT`](#lambda-parameters-limit)
  - [`LEAST-NEGATIVE-DOUBLE-FLOAT`](#least-negative-double-float)
  - [`LEAST-NEGATIVE-LONG-FLOAT`](#least-negative-long-float)
  - [`LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT`](#least-negative-normalized-double-float)
  - [`LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT`](#least-negative-normalized-long-float)
  - [`LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT`](#least-negative-normalized-short-float)
  - [`LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT`](#least-negative-normalized-single-float)
  - [`LEAST-NEGATIVE-SHORT-FLOAT`](#least-negative-short-float)
  - [`LEAST-NEGATIVE-SINGLE-FLOAT`](#least-negative-single-float)
  - [`LEAST-POSITIVE-DOUBLE-FLOAT`](#least-positive-double-float)
  - [`LEAST-POSITIVE-LONG-FLOAT`](#least-positive-long-float)
  - [`LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT`](#least-positive-normalized-double-float)
  - [`LEAST-POSITIVE-NORMALIZED-LONG-FLOAT`](#least-positive-normalized-long-float)
  - [`LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT`](#least-positive-normalized-short-float)
  - [`LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT`](#least-positive-normalized-single-float)
  - [`LEAST-POSITIVE-SHORT-FLOAT`](#least-positive-short-float)
  - [`LEAST-POSITIVE-SINGLE-FLOAT`](#least-positive-single-float)
  - [`LONG-FLOAT-EPSILON`](#long-float-epsilon)
  - [`LONG-FLOAT-NEGATIVE-EPSILON`](#long-float-negative-epsilon)
  - [`MOST-NEGATIVE-DOUBLE-FLOAT`](#most-negative-double-float)
  - [`MOST-NEGATIVE-FIXNUM`](#most-negative-fixnum)
  - [`MOST-NEGATIVE-LONG-FLOAT`](#most-negative-long-float)
  - [`MOST-NEGATIVE-SHORT-FLOAT`](#most-negative-short-float)
  - [`MOST-NEGATIVE-SINGLE-FLOAT`](#most-negative-single-float)
  - [`MOST-POSITIVE-DOUBLE-FLOAT`](#most-positive-double-float)
  - [`MOST-POSITIVE-FIXNUM`](#most-positive-fixnum)
  - [`MOST-POSITIVE-LONG-FLOAT`](#most-positive-long-float)
  - [`MOST-POSITIVE-SHORT-FLOAT`](#most-positive-short-float)
  - [`MOST-POSITIVE-SINGLE-FLOAT`](#most-positive-single-float)
  - [`MULTIPLE-VALUES-LIMIT`](#multiple-values-limit)
  - [`NIL`](#nil)
  - [`PI`](#pi)
  - [`SHORT-FLOAT-EPSILON`](#short-float-epsilon)
  - [`SHORT-FLOAT-NEGATIVE-EPSILON`](#short-float-negative-epsilon)
  - [`SINGLE-FLOAT-EPSILON`](#single-float-epsilon)
  - [`SINGLE-FLOAT-NEGATIVE-EPSILON`](#single-float-negative-epsilon)
  - [`T`](#t)
- [special operator](#special-operator)
  - [Symbol list](#symbol-list-6)
  - [`BLOCK`](#block)
  - [`CATCH`](#catch)
  - [`EVAL-WHEN`](#eval-when)
  - [`FLET`](#flet)
  - [`FUNCTION`](#function-1)
  - [`GO`](#go)
  - [`IF`](#if)
  - [`LABELS`](#labels)
  - [`LET`](#let)
  - [`LET*`](#let-1)
  - [`LOAD-TIME-VALUE`](#load-time-value)
  - [`LOCALLY`](#locally)
  - [`MACROLET`](#macrolet)
  - [`MULTIPLE-VALUE-CALL`](#multiple-value-call)
  - [`MULTIPLE-VALUE-PROG1`](#multiple-value-prog1)
  - [`PROGN`](#progn)
  - [`PROGV`](#progv)
  - [`QUOTE`](#quote)
  - [`RETURN-FROM`](#return-from)
  - [`SETQ`](#setq)
  - [`SYMBOL-MACROLET`](#symbol-macrolet)
  - [`TAGBODY`](#tagbody)
  - [`THE`](#the)
  - [`THROW`](#throw)
  - [`UNWIND-PROTECT`](#unwind-protect)
- [type-specifier](#type-specifier)
  - [Symbol list](#symbol-list-7)
  - [`BOOLEAN`](#boolean)
  - [`EQL`](#eql-1)
  - [`MOD`](#mod-1)
  - [`SIGNED-BYTE`](#signed-byte)
  - [`UNSIGNED-BYTE`](#unsigned-byte)


### `*`

```lisp
COMMON-LISP:*
  [symbol]

* names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the value of the most recent top level EVAL

* names a compiled function:
  Lambda-list: (&REST NUMBERS)
  Declared type: (FUNCTION (&REST NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Return the product of its arguments. With no args, returns 1.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `+`

```lisp
COMMON-LISP:+
  [symbol]

+ names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the value of the most recent top level READ

+ names a compiled function:
  Lambda-list: (&REST NUMBERS)
  Declared type: (FUNCTION (&REST NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Return the sum of its arguments. With no args, returns 0.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `-`

```lisp
COMMON-LISP:-
  [symbol]

- names a special variable:
  Declared type: T
  Declared always-bound.
  Value: NIL
  Documentation:
    the form currently being evaluated

- names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (NUMBER &REST NUMBER)
                  (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Subtract the second and all subsequent arguments from the first;
      or with one argument, negate the first argument.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `/`

```lisp
COMMON-LISP:/
  [symbol]

/ names a special variable:
  Declared type: LIST
  Declared always-bound.
  Value: NIL
  Documentation:
    a list of all the values returned by the most recent top level EVAL

/ names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (NUMBER &REST NUMBER)
                  (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Divide the first argument by each of the following arguments, in turn.
      With one argument, return reciprocal.
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `/=`

```lisp
COMMON-LISP:/=
  [symbol]

/= names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (NUMBER &REST NUMBER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (NUMBER &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if no two of its arguments are numerically equal, NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `1+`

```lisp
COMMON-LISP:1+
  [symbol]

1+ names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Return NUMBER + 1.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `1-`

```lisp
COMMON-LISP:1-
  [symbol]

1- names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Return NUMBER - 1.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `<`

```lisp
COMMON-LISP:<
  [symbol]

< names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (REAL &REST REAL) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if its arguments are in strictly increasing order, NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `<=`

```lisp
COMMON-LISP:<=
  [symbol]

<= names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (REAL &REST REAL) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if arguments are in strictly non-decreasing order, NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `=`

```lisp
COMMON-LISP:=
  [symbol]

= names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (NUMBER &REST NUMBER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (NUMBER &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if all of its arguments are numerically equal, NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `>`

```lisp
COMMON-LISP:>
  [symbol]

> names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (REAL &REST REAL) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if its arguments are in strictly decreasing order, NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `>=`

```lisp
COMMON-LISP:>=
  [symbol]

>= names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (REAL &REST REAL) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if arguments are in strictly non-increasing order, NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `ABORT`

```lisp
COMMON-LISP:ABORT
  [symbol]

ABORT names a compiled function:
  Lambda-list: (&OPTIONAL CONDITION)
  Declared type: (FUNCTION (&OPTIONAL (OR CONDITION NULL)) NIL)
  Documentation:
    Transfer control to a restart named ABORT, signalling a CONTROL-ERROR if
       none exists.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `ABS`

```lisp
COMMON-LISP:ABS
  [symbol]

ABS names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (REAL 0) &OPTIONAL))
  Documentation:
    Return the absolute value of the number.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `ACONS`

```lisp
COMMON-LISP:ACONS
  [symbol]

ACONS names a compiled function:
  Lambda-list: (KEY DATUM ALIST)
  Declared type: (FUNCTION (T T T) (VALUES CONS &OPTIONAL))
  Documentation:
    Construct a new alist by adding the pair (KEY . DATUM) to ALIST.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `ACOS`

```lisp
COMMON-LISP:ACOS
  [symbol]

ACOS names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the arc cosine of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `ACOSH`

```lisp
COMMON-LISP:ACOSH
  [symbol]

ACOSH names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the hyperbolic arc cosine of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `ADJOIN`

```lisp
COMMON-LISP:ADJOIN
  [symbol]

ADJOIN names a compiled function:
  Lambda-list: (ITEM LIST &KEY KEY (TEST (FUNCTION EQL) TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES CONS &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Add ITEM to LIST unless it is already a member
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `ADJUST-ARRAY`

```lisp
COMMON-LISP:ADJUST-ARRAY
  [symbol]

ADJUST-ARRAY names a compiled function:
  Lambda-list: (ARRAY DIMENSIONS &KEY
                (ELEMENT-TYPE (ARRAY-ELEMENT-TYPE ARRAY)
                 ELEMENT-TYPE-P)
                (INITIAL-ELEMENT NIL ELEMENT-P)
                (INITIAL-CONTENTS NIL CONTENTS-P) FILL-POINTER
                DISPLACED-TO (DISPLACED-INDEX-OFFSET 0 OFFSET-P))
  Declared type: (FUNCTION
                  (ARRAY (OR LIST (UNSIGNED-BYTE 45)) &KEY
                   (:ELEMENT-TYPE
                    (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS))
                   (:INITIAL-ELEMENT T) (:INITIAL-CONTENTS T)
                   (:FILL-POINTER (OR (UNSIGNED-BYTE 45) BOOLEAN))
                   (:DISPLACED-TO (OR ARRAY NULL))
                   (:DISPLACED-INDEX-OFFSET (UNSIGNED-BYTE 45)))
                  (VALUES ARRAY &OPTIONAL))
  Documentation:
    Adjust ARRAY's dimensions to the given DIMENSIONS and stuff.
  Known attributes: important-result
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ADJUSTABLE-ARRAY-P`

```lisp
COMMON-LISP:ADJUSTABLE-ARRAY-P
  [symbol]

ADJUSTABLE-ARRAY-P names a compiled function:
  Lambda-list: (ARRAY)
  Declared type: (FUNCTION (ARRAY) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if and only if calling ADJUST-ARRAY on ARRAY will return
       the identical object.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ALPHA-CHAR-P`

```lisp
COMMON-LISP:ALPHA-CHAR-P
  [symbol]

ALPHA-CHAR-P names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    The argument must be a character object. ALPHA-CHAR-P returns T if the
    argument is an alphabetic character, A-Z or a-z; otherwise NIL.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `ALPHANUMERICP`

```lisp
COMMON-LISP:ALPHANUMERICP
  [symbol]

ALPHANUMERICP names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Given a character-object argument, ALPHANUMERICP returns T if the argument
    is either numeric or alphabetic.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `APPEND`

```lisp
COMMON-LISP:APPEND
  [symbol]

APPEND names a compiled function:
  Lambda-list: (&REST LISTS)
  Declared type: (FUNCTION * (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES T &OPTIONAL))
  Documentation:
    Construct and return a list by concatenating LISTS.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `APPLY`

```lisp
COMMON-LISP:APPLY
  [symbol]

APPLY names a compiled function:
  Lambda-list: (FUNCTION ARG &REST ARGUMENTS)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) T &REST T) *)
  Documentation:
    Apply FUNCTION to a list of arguments produced by evaluating ARGUMENTS in
      the manner of LIST*. That is, a list is made of the values of all but the
      last argument, appended to the value of the last argument, which must be a
      list.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;EVAL.LISP

(SETF APPLY) has a complex setf-expansion:
  Lambda-list: (FUNCTIONOID &REST ARGS)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `APROPOS`

```lisp
COMMON-LISP:APROPOS
  [symbol]

APROPOS names a compiled function:
  Lambda-list: (STRING-DESIGNATOR &OPTIONAL PACKAGE EXTERNAL-ONLY)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE) T)
                  (VALUES &OPTIONAL))
  Documentation:
    Briefly describe all symbols which contain the specified STRING.
      If PACKAGE is supplied then only describe symbols present in
      that package. If EXTERNAL-ONLY then only describe
      external symbols in the specified package.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;DEFPACKAGE.LISP

```

### `APROPOS-LIST`

```lisp
COMMON-LISP:APROPOS-LIST
  [symbol]

APROPOS-LIST names a compiled function:
  Lambda-list: (STRING-DESIGNATOR &OPTIONAL PACKAGE-DESIGNATOR
                EXTERNAL-ONLY &AUX
                (STRING
                 (THE SIMPLE-STRING
                      (STRINGIFY-STRING-DESIGNATOR STRING-DESIGNATOR))))
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE) T)
                  (VALUES LIST &OPTIONAL))
  Documentation:
    Like APROPOS, except that it returns a list of the symbols found instead
      of describing them.
  Known attributes: flushable, unsafely-flushable, recursive
  Source file: SYS:SRC;CODE;DEFPACKAGE.LISP

```

### `AREF`

```lisp
COMMON-LISP:AREF
  [symbol]

AREF names a compiled function:
  Lambda-list: (ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION (ARRAY &REST (UNSIGNED-BYTE 45))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (ARRAY &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Return the element of the ARRAY specified by the SUBSCRIPTS.
  Known attributes: foldable
  Source file: SYS:SRC;CODE;ARRAY.LISP

(SETF AREF) names a compiled function:
  Lambda-list: (NEW-VALUE ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION (T ARRAY &REST (UNSIGNED-BYTE 45))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T ARRAY &REST T) (VALUES T &OPTIONAL))
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `(SETF AREF)`

```lisp
COMMON-LISP-USER::|(SETF AREF)|
  [symbol]

```

### `ARITHMETIC-ERROR-OPERANDS`

```lisp
COMMON-LISP:ARITHMETIC-ERROR-OPERANDS
  [symbol]

ARITHMETIC-ERROR-OPERANDS names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `ARITHMETIC-ERROR-OPERATION`

```lisp
COMMON-LISP:ARITHMETIC-ERROR-OPERATION
  [symbol]

ARITHMETIC-ERROR-OPERATION names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `ARRAY-DIMENSION`

```lisp
COMMON-LISP:ARRAY-DIMENSION
  [symbol]

ARRAY-DIMENSION names a compiled function:
  Lambda-list: (ARRAY AXIS-NUMBER)
  Declared type: (FUNCTION (ARRAY (MOD 129))
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Documentation:
    Return the length of dimension AXIS-NUMBER of ARRAY.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAY-DIMENSIONS`

```lisp
COMMON-LISP:ARRAY-DIMENSIONS
  [symbol]

ARRAY-DIMENSIONS names a compiled function:
  Lambda-list: (ARRAY)
  Declared type: (FUNCTION (ARRAY) (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES LIST &OPTIONAL))
  Documentation:
    Return a list whose elements are the dimensions of the array
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAY-DISPLACEMENT`

```lisp
COMMON-LISP:ARRAY-DISPLACEMENT
  [symbol]

ARRAY-DISPLACEMENT names a compiled function:
  Lambda-list: (ARRAY)
  Derived type: (FUNCTION (ARRAY)
                 (VALUES (OR ARRAY NULL) (UNSIGNED-BYTE 45) &OPTIONAL))
  Documentation:
    Return the values of :DISPLACED-TO and :DISPLACED-INDEX-offset
       options to MAKE-ARRAY, or NIL and 0 if not a displaced array.
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAY-ELEMENT-TYPE`

```lisp
COMMON-LISP:ARRAY-ELEMENT-TYPE
  [symbol]

ARRAY-ELEMENT-TYPE names a compiled function:
  Lambda-list: (ARRAY)
  Declared type: (FUNCTION (ARRAY) (VALUES (OR CONS SYMBOL) &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (OR CONS SYMBOL) &OPTIONAL))
  Documentation:
    Return the type of the elements of the array
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAY-HAS-FILL-POINTER-P`

```lisp
COMMON-LISP:ARRAY-HAS-FILL-POINTER-P
  [symbol]

ARRAY-HAS-FILL-POINTER-P names a compiled function:
  Lambda-list: (ARRAY)
  Declared type: (FUNCTION (ARRAY) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the given ARRAY has a fill pointer, or NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAY-IN-BOUNDS-P`

```lisp
COMMON-LISP:ARRAY-IN-BOUNDS-P
  [symbol]

ARRAY-IN-BOUNDS-P names a compiled function:
  Lambda-list: (ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION (ARRAY &REST INTEGER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (ARRAY &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the SUBSCRIPTS are in bounds for the ARRAY, NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAY-RANK`

```lisp
COMMON-LISP:ARRAY-RANK
  [symbol]

ARRAY-RANK names a compiled function:
  Lambda-list: (ARRAY)
  Declared type: (FUNCTION (ARRAY) (VALUES (MOD 129) &OPTIONAL))
  Documentation:
    Return the number of dimensions of ARRAY.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAY-ROW-MAJOR-INDEX`

```lisp
COMMON-LISP:ARRAY-ROW-MAJOR-INDEX
  [symbol]

ARRAY-ROW-MAJOR-INDEX names a compiled function:
  Lambda-list: (ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION (ARRAY &REST (UNSIGNED-BYTE 45))
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION (ARRAY &REST T)
                 (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAY-TOTAL-SIZE`

```lisp
COMMON-LISP:ARRAY-TOTAL-SIZE
  [symbol]

ARRAY-TOTAL-SIZE names a compiled function:
  Lambda-list: (ARRAY)
  Declared type: (FUNCTION (ARRAY)
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Documentation:
    Return the total number of elements in the Array.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `ARRAYP`

```lisp
COMMON-LISP:ARRAYP
  [symbol]

ARRAYP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is an ARRAY, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `ASH`

```lisp
COMMON-LISP:ASH
  [symbol]

ASH names a compiled function:
  Lambda-list: (INTEGER COUNT)
  Declared type: (FUNCTION (INTEGER INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES (OR NULL INTEGER) &OPTIONAL))
  Documentation:
    Shifts integer left by count places preserving sign. - count shifts right.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `ASIN`

```lisp
COMMON-LISP:ASIN
  [symbol]

ASIN names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the arc sine of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `ASINH`

```lisp
COMMON-LISP:ASINH
  [symbol]

ASINH names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the hyperbolic arc sine of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `ASSOC`

```lisp
COMMON-LISP:ASSOC
  [symbol]

ASSOC names a compiled function:
  Lambda-list: (ITEM ALIST &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Return the cons in ALIST whose car is equal (by a given test or EQL) to
       the ITEM.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `ASSOC-IF`

```lisp
COMMON-LISP:ASSOC-IF
  [symbol]

ASSOC-IF names a compiled function:
  Lambda-list: (PREDICATE ALIST &KEY KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) LIST &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &KEY (:KEY T)) (VALUES LIST &OPTIONAL))
  Documentation:
    Return the first cons in ALIST whose CAR satisfies PREDICATE. If
       KEY is supplied, apply it to the CAR of each cons before testing.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `ASSOC-IF-NOT`

```lisp
COMMON-LISP:ASSOC-IF-NOT
  [symbol]

ASSOC-IF-NOT names a compiled function:
  Lambda-list: (PREDICATE ALIST &KEY KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) LIST &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &KEY (:KEY T)) (VALUES LIST &OPTIONAL))
  Documentation:
    Return the first cons in ALIST whose CAR does not satisfy PREDICATE.
      If KEY is supplied, apply it to the CAR of each cons before testing.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `ATAN`

```lisp
COMMON-LISP:ATAN
  [symbol]

ATAN names a compiled function:
  Lambda-list: (Y &OPTIONAL (X NIL XP))
  Declared type: (FUNCTION (NUMBER &OPTIONAL REAL)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) *)
  Documentation:
    Return the arc tangent of Y if X is omitted or Y/X if X is supplied.
  Known attributes: foldable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `ATANH`

```lisp
COMMON-LISP:ATANH
  [symbol]

ATANH names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the hyperbolic arc tangent of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `ATOM`

```lisp
COMMON-LISP:ATOM
  [symbol]

ATOM names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is an ATOM, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;PRED.LISP

ATOM names a primitive type-specifier:
  Lambda-list: ()

```

### `BIT`

```lisp
COMMON-LISP:BIT
  [symbol]

BIT names a compiled function:
  Lambda-list: (BIT-ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION ((ARRAY BIT) &REST (UNSIGNED-BYTE 45))
                  (VALUES BIT &OPTIONAL))
  Derived type: (FUNCTION ((ARRAY BIT) &REST T) (VALUES BIT &OPTIONAL))
  Documentation:
    Return the bit from the BIT-ARRAY at the specified SUBSCRIPTS.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

(SETF BIT) names a compiled function:
  Lambda-list: (NEW-VALUE BIT-ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION (BIT (ARRAY BIT) &REST (UNSIGNED-BYTE 45))
                  (VALUES BIT &OPTIONAL))
  Derived type: (FUNCTION (BIT (ARRAY BIT) &REST T)
                 (VALUES BIT &OPTIONAL))
  Source file: SYS:SRC;CODE;ARRAY.LISP

BIT names a primitive type-specifier:
  Lambda-list: ()

```

### `(SETF BIT)`

```lisp
COMMON-LISP-USER::|(SETF BIT)|
  [symbol]

```

### `BIT-AND`

```lisp
COMMON-LISP:BIT-AND
  [symbol]

BIT-AND names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGAND on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-ANDC1`

```lisp
COMMON-LISP:BIT-ANDC1
  [symbol]

BIT-ANDC1 names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGANDC1 on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-ANDC2`

```lisp
COMMON-LISP:BIT-ANDC2
  [symbol]

BIT-ANDC2 names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGANDC2 on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-EQV`

```lisp
COMMON-LISP:BIT-EQV
  [symbol]

BIT-EQV names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGEQV on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-IOR`

```lisp
COMMON-LISP:BIT-IOR
  [symbol]

BIT-IOR names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGIOR on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-NAND`

```lisp
COMMON-LISP:BIT-NAND
  [symbol]

BIT-NAND names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGNAND on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-NOR`

```lisp
COMMON-LISP:BIT-NOR
  [symbol]

BIT-NOR names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGNOR on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-NOT`

```lisp
COMMON-LISP:BIT-NOT
  [symbol]

BIT-NOT names a compiled function:
  Lambda-list: (BIT-ARRAY &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) &OPTIONAL (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Performs a bit-wise logical NOT on the elements of BIT-ARRAY,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. Both arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-ORC1`

```lisp
COMMON-LISP:BIT-ORC1
  [symbol]

BIT-ORC1 names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGORC1 on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-ORC2`

```lisp
COMMON-LISP:BIT-ORC2
  [symbol]

BIT-ORC2 names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGORC2 on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BIT-VECTOR-P`

```lisp
COMMON-LISP:BIT-VECTOR-P
  [symbol]

BIT-VECTOR-P names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a BIT-VECTOR, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `BIT-XOR`

```lisp
COMMON-LISP:BIT-XOR
  [symbol]

BIT-XOR names a compiled function:
  Lambda-list: (BIT-ARRAY-1 BIT-ARRAY-2 &OPTIONAL RESULT-BIT-ARRAY)
  Declared type: (FUNCTION
                  ((ARRAY BIT) (ARRAY BIT) &OPTIONAL
                   (OR (ARRAY BIT) BOOLEAN))
                  (VALUES (ARRAY BIT) &OPTIONAL))
  Documentation:
    Perform a bit-wise LOGXOR on the elements of BIT-ARRAY-1 and BIT-ARRAY-2,
      putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
      BIT-ARRAY-1 is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
      created. All the arrays must have the same rank and dimensions.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `BOOLE`

```lisp
COMMON-LISP:BOOLE
  [symbol]

BOOLE names a compiled function:
  Lambda-list: (OP INTEGER1 INTEGER2)
  Declared type: (FUNCTION ((UNSIGNED-BYTE 4) INTEGER INTEGER)
                  (VALUES INTEGER &OPTIONAL))
  Documentation:
    Bit-wise boolean function on two integers. Function chosen by OP:
            0       BOOLE-CLR
            1       BOOLE-SET
            2       BOOLE-1
            3       BOOLE-2
            4       BOOLE-C1
            5       BOOLE-C2
            6       BOOLE-AND
            7       BOOLE-IOR
            8       BOOLE-XOR
            9       BOOLE-EQV
            10      BOOLE-NAND
            11      BOOLE-NOR
            12      BOOLE-ANDC1
            13      BOOLE-ANDC2
            14      BOOLE-ORC1
            15      BOOLE-ORC2
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `BOTH-CASE-P`

```lisp
COMMON-LISP:BOTH-CASE-P
  [symbol]

BOTH-CASE-P names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    The argument must be a character object. BOTH-CASE-P returns T if the
    argument is an alphabetic character and if the character exists in both upper
    and lower case. For ASCII, this is the same as ALPHA-CHAR-P.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `BOUNDP`

```lisp
COMMON-LISP:BOUNDP
  [symbol]

BOUNDP names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return non-NIL if SYMBOL is bound to a value.
  Known attributes: flushable, unsafely-flushable, predicate
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `BREAK`

```lisp
COMMON-LISP:BREAK
  [symbol]

BREAK names a compiled function:
  Lambda-list: (&OPTIONAL (DATUM break) &REST ARGUMENTS)
  Declared type: (FUNCTION (&OPTIONAL (OR STRING FUNCTION) &REST T)
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Print a message and invoke the debugger without allowing any possibility
    of condition handling occurring.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;COLD-ERROR.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION BREA..

```

### `BROADCAST-STREAM-STREAMS`

```lisp
COMMON-LISP:BROADCAST-STREAM-STREAMS
  [symbol]

BROADCAST-STREAM-STREAMS names a compiled function:
  Lambda-list: (INSTANCE)
  Derived type: (FUNCTION (BROADCAST-STREAM) (VALUES LIST &OPTIONAL))
  Source file: SYS:SRC;CODE;ANSI-STREAM.LISP

```

### `BUTLAST`

```lisp
COMMON-LISP:BUTLAST
  [symbol]

BUTLAST names a compiled function:
  Lambda-list: (LIST &OPTIONAL (N 1))
  Declared type: (FUNCTION (LIST &OPTIONAL UNSIGNED-BYTE)
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (LIST &OPTIONAL T) (VALUES LIST &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `BYTE`

```lisp
COMMON-LISP:BYTE
  [symbol]

BYTE names a compiled function:
  Lambda-list: (SIZE POSITION)
  Declared type: (FUNCTION ((UNSIGNED-BYTE 38) (UNSIGNED-BYTE 38))
                  (VALUES CONS &OPTIONAL))
  Documentation:
    Return a byte specifier which may be used by other byte functions
      (e.g. LDB).
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `BYTE-POSITION`

```lisp
COMMON-LISP:BYTE-POSITION
  [symbol]

BYTE-POSITION names a compiled function:
  Lambda-list: (BYTESPEC)
  Declared type: (FUNCTION (CONS) (VALUES (UNSIGNED-BYTE 38) &OPTIONAL))
  Documentation:
    Return the position part of the byte specifier bytespec.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `BYTE-SIZE`

```lisp
COMMON-LISP:BYTE-SIZE
  [symbol]

BYTE-SIZE names a compiled function:
  Lambda-list: (BYTESPEC)
  Declared type: (FUNCTION (CONS) (VALUES (UNSIGNED-BYTE 38) &OPTIONAL))
  Documentation:
    Return the size part of the byte specifier bytespec.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `CAAAAR`

```lisp
COMMON-LISP:CAAAAR
  [symbol]

CAAAAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the caaar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CAAAAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CAAAAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CAAAAR)`

```lisp
COMMON-LISP-USER::|(SETF CAAAAR)|
  [symbol]

```

### `CAAADR`

```lisp
COMMON-LISP:CAAADR
  [symbol]

CAAADR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the caadr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CAAADR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CAAADR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CAAADR)`

```lisp
COMMON-LISP-USER::|(SETF CAAADR)|
  [symbol]

```

### `CAAAR`

```lisp
COMMON-LISP:CAAAR
  [symbol]

CAAAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 1st object in the caar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CAAAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CAAAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CAAAR)`

```lisp
COMMON-LISP-USER::|(SETF CAAAR)|
  [symbol]

```

### `CAADAR`

```lisp
COMMON-LISP:CAADAR
  [symbol]

CAADAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the cadar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CAADAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CAADAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CAADAR)`

```lisp
COMMON-LISP-USER::|(SETF CAADAR)|
  [symbol]

```

### `CAADDR`

```lisp
COMMON-LISP:CAADDR
  [symbol]

CAADDR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the caddr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CAADDR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CAADDR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CAADDR)`

```lisp
COMMON-LISP-USER::|(SETF CAADDR)|
  [symbol]

```

### `CAADR`

```lisp
COMMON-LISP:CAADR
  [symbol]

CAADR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 1st object in the cadr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CAADR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CAADR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CAADR)`

```lisp
COMMON-LISP-USER::|(SETF CAADR)|
  [symbol]

```

### `CAAR`

```lisp
COMMON-LISP:CAAR
  [symbol]

CAAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the 1st sublist.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CAAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CAAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CAAR)`

```lisp
COMMON-LISP-USER::|(SETF CAAR)|
  [symbol]

```

### `CADAAR`

```lisp
COMMON-LISP:CADAAR
  [symbol]

CADAAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the cdaar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CADAAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CADAAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CADAAR)`

```lisp
COMMON-LISP-USER::|(SETF CADAAR)|
  [symbol]

```

### `CADADR`

```lisp
COMMON-LISP:CADADR
  [symbol]

CADADR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the cdadr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CADADR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CADADR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CADADR)`

```lisp
COMMON-LISP-USER::|(SETF CADADR)|
  [symbol]

```

### `CADAR`

```lisp
COMMON-LISP:CADAR
  [symbol]

CADAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the cdar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CADAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CADAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CADAR)`

```lisp
COMMON-LISP-USER::|(SETF CADAR)|
  [symbol]

```

### `CADDAR`

```lisp
COMMON-LISP:CADDAR
  [symbol]

CADDAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the cddar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CADDAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CADDAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CADDAR)`

```lisp
COMMON-LISP-USER::|(SETF CADDAR)|
  [symbol]

```

### `CADDDR`

```lisp
COMMON-LISP:CADDDR
  [symbol]

CADDDR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the car of the cdddr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CADDDR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CADDDR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CADDDR)`

```lisp
COMMON-LISP-USER::|(SETF CADDDR)|
  [symbol]

```

### `CADDR`

```lisp
COMMON-LISP:CADDR
  [symbol]

CADDR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 1st object in the cddr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CADDR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CADDR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CADDR)`

```lisp
COMMON-LISP-USER::|(SETF CADDR)|
  [symbol]

```

### `CADR`

```lisp
COMMON-LISP:CADR
  [symbol]

CADR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 2nd object in a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CADR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CADR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CADR)`

```lisp
COMMON-LISP-USER::|(SETF CADR)|
  [symbol]

```

### `CAR`

```lisp
COMMON-LISP:CAR
  [symbol]

CAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 1st object in a list.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CAR) has setf-expansion: SB-KERNEL:%RPLACA

```

### `(SETF CAR)`

```lisp
COMMON-LISP-USER::|(SETF CAR)|
  [symbol]

```

### `CDAAAR`

```lisp
COMMON-LISP:CDAAAR
  [symbol]

CDAAAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the caaar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDAAAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDAAAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDAAAR)`

```lisp
COMMON-LISP-USER::|(SETF CDAAAR)|
  [symbol]

```

### `CDAADR`

```lisp
COMMON-LISP:CDAADR
  [symbol]

CDAADR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the caadr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDAADR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDAADR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDAADR)`

```lisp
COMMON-LISP-USER::|(SETF CDAADR)|
  [symbol]

```

### `CDAAR`

```lisp
COMMON-LISP:CDAAR
  [symbol]

CDAAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the caar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDAAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDAAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDAAR)`

```lisp
COMMON-LISP-USER::|(SETF CDAAR)|
  [symbol]

```

### `CDADAR`

```lisp
COMMON-LISP:CDADAR
  [symbol]

CDADAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the cadar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDADAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDADAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDADAR)`

```lisp
COMMON-LISP-USER::|(SETF CDADAR)|
  [symbol]

```

### `CDADDR`

```lisp
COMMON-LISP:CDADDR
  [symbol]

CDADDR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the caddr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDADDR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDADDR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDADDR)`

```lisp
COMMON-LISP-USER::|(SETF CDADDR)|
  [symbol]

```

### `CDADR`

```lisp
COMMON-LISP:CDADR
  [symbol]

CDADR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the cadr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDADR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDADR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDADR)`

```lisp
COMMON-LISP-USER::|(SETF CDADR)|
  [symbol]

```

### `CDAR`

```lisp
COMMON-LISP:CDAR
  [symbol]

CDAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the 1st sublist.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDAR)`

```lisp
COMMON-LISP-USER::|(SETF CDAR)|
  [symbol]

```

### `CDDAAR`

```lisp
COMMON-LISP:CDDAAR
  [symbol]

CDDAAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the cdaar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDDAAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDDAAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDDAAR)`

```lisp
COMMON-LISP-USER::|(SETF CDDAAR)|
  [symbol]

```

### `CDDADR`

```lisp
COMMON-LISP:CDDADR
  [symbol]

CDDADR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the cdadr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDDADR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDDADR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDDADR)`

```lisp
COMMON-LISP-USER::|(SETF CDDADR)|
  [symbol]

```

### `CDDAR`

```lisp
COMMON-LISP:CDDAR
  [symbol]

CDDAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the cdar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDDAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDDAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDDAR)`

```lisp
COMMON-LISP-USER::|(SETF CDDAR)|
  [symbol]

```

### `CDDDAR`

```lisp
COMMON-LISP:CDDDAR
  [symbol]

CDDDAR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the cddar of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDDDAR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDDDAR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDDDAR)`

```lisp
COMMON-LISP-USER::|(SETF CDDDAR)|
  [symbol]

```

### `CDDDDR`

```lisp
COMMON-LISP:CDDDDR
  [symbol]

CDDDDR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the cdddr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDDDDR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDDDDR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDDDDR)`

```lisp
COMMON-LISP-USER::|(SETF CDDDDR)|
  [symbol]

```

### `CDDDR`

```lisp
COMMON-LISP:CDDDR
  [symbol]

CDDDR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the cdr of the cddr of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDDDR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDDDR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDDDR)`

```lisp
COMMON-LISP-USER::|(SETF CDDDR)|
  [symbol]

```

### `CDDR`

```lisp
COMMON-LISP:CDDR
  [symbol]

CDDR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return all but the 1st two objects of a list.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDDR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDDR) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF CDDR)`

```lisp
COMMON-LISP-USER::|(SETF CDDR)|
  [symbol]

```

### `CDR`

```lisp
COMMON-LISP:CDR
  [symbol]

CDR names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return all but the first object in a list.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF CDR) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T CONS) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CDR) has setf-expansion: SB-KERNEL:%RPLACD

```

### `(SETF CDR)`

```lisp
COMMON-LISP-USER::|(SETF CDR)|
  [symbol]

```

### `CEILING`

```lisp
COMMON-LISP:CEILING
  [symbol]

CEILING names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES INTEGER REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR NULL INTEGER) NUMBER &OPTIONAL))
  Documentation:
    Return number (or number/divisor) as an integer, rounded toward 0.
      The second returned value is the remainder.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `CELL-ERROR-NAME`

```lisp
COMMON-LISP:CELL-ERROR-NAME
  [symbol]

CELL-ERROR-NAME names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `CERROR`

```lisp
COMMON-LISP:CERROR
  [symbol]

CERROR names a compiled function:
  Lambda-list: (CONTINUE-STRING DATUM &REST ARGUMENTS)
  Declared type: (FUNCTION
                  ((OR STRING FUNCTION)
                   (OR STRING FUNCTION SYMBOL CONDITION
                       SB-PCL::CONDITION-CLASS)
                   &REST T)
                  (VALUES NULL &OPTIONAL))
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;COLD-ERROR.LISP

```

### `CHAR`

```lisp
COMMON-LISP:CHAR
  [symbol]

CHAR names a compiled function:
  Lambda-list: (STRING INDEX)
  Declared type: (FUNCTION (STRING (UNSIGNED-BYTE 45))
                  (VALUES CHARACTER &OPTIONAL))
  Documentation:
    Given a string and a non-negative integer index less than the length of
      the string, returns the character object representing the character at
      that position in the string.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

(SETF CHAR) names a compiled function:
  Lambda-list: (NEWVAL STRING INDEX)
  Derived type: (FUNCTION (CHARACTER STRING (UNSIGNED-BYTE 45))
                 (VALUES CHARACTER &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF CHAR) has setf-expansion: SB-KERNEL:%CHARSET

```

### `(SETF CHAR)`

```lisp
COMMON-LISP-USER::|(SETF CHAR)|
  [symbol]

```

### `CHAR-CODE`

```lisp
COMMON-LISP:CHAR-CODE
  [symbol]

CHAR-CODE names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES (MOD 1114112) &OPTIONAL))
  Documentation:
    Return the integer code of CHAR.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-DOWNCASE`

```lisp
COMMON-LISP:CHAR-DOWNCASE
  [symbol]

CHAR-DOWNCASE names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES CHARACTER &OPTIONAL))
  Documentation:
    Return CHAR converted to lower-case if that is possible.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-EQUAL`

```lisp
COMMON-LISP:CHAR-EQUAL
  [symbol]

CHAR-EQUAL names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if all of the arguments are the same character.
    Case is ignored.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-GREATERP`

```lisp
COMMON-LISP:CHAR-GREATERP
  [symbol]

CHAR-GREATERP names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the arguments are in strictly decreasing alphabetic order.
    Case is ignored.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-INT`

```lisp
COMMON-LISP:CHAR-INT
  [symbol]

CHAR-INT names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES (MOD 1114112) &OPTIONAL))
  Documentation:
    Return the integer code of CHAR. (In SBCL this is the same as CHAR-CODE, as
    there are no character bits or fonts.)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-LESSP`

```lisp
COMMON-LISP:CHAR-LESSP
  [symbol]

CHAR-LESSP names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the arguments are in strictly increasing alphabetic order.
    Case is ignored.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-NAME`

```lisp
COMMON-LISP:CHAR-NAME
  [symbol]

CHAR-NAME names a compiled function:
  Lambda-list: (CHARACTER)
  Declared type: (FUNCTION (CHARACTER)
                  (VALUES (OR SIMPLE-BASE-STRING NULL) &OPTIONAL))
  Derived type: (FUNCTION (CHARACTER)
                 (VALUES SIMPLE-BASE-STRING &OPTIONAL))
  Documentation:
    Return the name (a STRING) for a CHARACTER object.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-UNICODE.LISP

```

### `CHAR-NOT-EQUAL`

```lisp
COMMON-LISP:CHAR-NOT-EQUAL
  [symbol]

CHAR-NOT-EQUAL names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (CHARACTER &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if no two of the arguments are the same character.
    Case is ignored.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-NOT-GREATERP`

```lisp
COMMON-LISP:CHAR-NOT-GREATERP
  [symbol]

CHAR-NOT-GREATERP names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the arguments are in strictly non-decreasing alphabetic order.
    Case is ignored.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-NOT-LESSP`

```lisp
COMMON-LISP:CHAR-NOT-LESSP
  [symbol]

CHAR-NOT-LESSP names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the arguments are in strictly non-increasing alphabetic order.
    Case is ignored.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR-UPCASE`

```lisp
COMMON-LISP:CHAR-UPCASE
  [symbol]

CHAR-UPCASE names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES CHARACTER &OPTIONAL))
  Documentation:
    Return CHAR converted to upper-case if that is possible. Don't convert
    lowercase eszet (U+DF).
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR/=`

```lisp
COMMON-LISP:CHAR/=
  [symbol]

CHAR/= names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (CHARACTER &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if no two of the arguments are the same character.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR<`

```lisp
COMMON-LISP:CHAR<
  [symbol]

CHAR< names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (CHARACTER &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the arguments are in strictly increasing alphabetic order.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR<=`

```lisp
COMMON-LISP:CHAR<=
  [symbol]

CHAR<= names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (CHARACTER &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the arguments are in strictly non-decreasing alphabetic order.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR=`

```lisp
COMMON-LISP:CHAR=
  [symbol]

CHAR= names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (CHARACTER &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if all of the arguments are the same character.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate, commutative
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR>`

```lisp
COMMON-LISP:CHAR>
  [symbol]

CHAR> names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (CHARACTER &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the arguments are in strictly decreasing alphabetic order.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHAR>=`

```lisp
COMMON-LISP:CHAR>=
  [symbol]

CHAR>= names a compiled function:
  Lambda-list: (CHARACTER &REST MORE-CHARACTERS)
  Declared type: (FUNCTION (CHARACTER &REST CHARACTER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (CHARACTER &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if the arguments are in strictly non-increasing alphabetic order.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `CHARACTER`

```lisp
COMMON-LISP:CHARACTER
  [symbol]

CHARACTER names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES CHARACTER &OPTIONAL))
  Documentation:
    Coerce OBJECT into a CHARACTER if possible. Legal inputs are characters,
    strings and symbols of length 1.
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

CHARACTER names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:CHARACTER>:
  Class precedence-list: CHARACTER, T
  Direct superclasses: T
  No subclasses.
  Sealed.
  No direct slots.

```

### `CHARACTERP`

```lisp
COMMON-LISP:CHARACTERP
  [symbol]

CHARACTERP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a CHARACTER, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `CIS`

```lisp
COMMON-LISP:CIS
  [symbol]

CIS names a compiled function:
  Lambda-list: (THETA)
  Declared type: (FUNCTION (REAL)
                  (VALUES
                   (OR (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    Return cos(Theta) + i sin(Theta), i.e. exp(i Theta).
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `CLASS-OF`

```lisp
COMMON-LISP:CLASS-OF
  [symbol]

CLASS-OF names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION (T) (VALUES CLASS &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;PCL;BRAID.LISP

```

### `CLEAR-INPUT`

```lisp
COMMON-LISP:CLEAR-INPUT
  [symbol]

CLEAR-INPUT names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-INPUT*))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T) (VALUES NULL &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `CLEAR-OUTPUT`

```lisp
COMMON-LISP:CLEAR-OUTPUT
  [symbol]

CLEAR-OUTPUT names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T) (VALUES NULL &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `CLRHASH`

```lisp
COMMON-LISP:CLRHASH
  [symbol]

CLRHASH names a compiled function:
  Lambda-list: (HASH-TABLE)
  Declared type: (FUNCTION (HASH-TABLE) (VALUES HASH-TABLE &OPTIONAL))
  Documentation:
    This removes all the entries from HASH-TABLE and returns the hash
    table itself.
  Source file: SYS:SRC;CODE;TARGET-HASH-TABLE.LISP

```

### `CODE-CHAR`

```lisp
COMMON-LISP:CODE-CHAR
  [symbol]

CODE-CHAR names a compiled function:
  Lambda-list: (CODE)
  Declared type: (FUNCTION ((MOD 1114112)) (VALUES CHARACTER &OPTIONAL))
  Documentation:
    Return the character with the code CODE.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `COERCE`

```lisp
COMMON-LISP:COERCE
  [symbol]

COERCE names a compiled function:
  Lambda-list: (OBJECT OUTPUT-TYPE-SPEC)
  Declared type: (FUNCTION
                  (T (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T T) *)
  Documentation:
    Coerce the Object to an object of type Output-Type-Spec.
  Known attributes: movable
  Source file: SYS:SRC;CODE;COERCE.LISP

```

### `COMPILE`

```lisp
COMMON-LISP:COMPILE
  [symbol]

COMPILE names a compiled function:
  Lambda-list: (NAME &OPTIONAL
                (DEFINITION
                 (OR (AND (SYMBOLP NAME) (MACRO-FUNCTION NAME))
                     (FDEFINITION NAME))))
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL) &OPTIONAL (OR LIST FUNCTION))
                  (VALUES (OR FUNCTION SYMBOL CONS) BOOLEAN BOOLEAN
                          &OPTIONAL))
  Documentation:
    Produce a compiled function from DEFINITION. If DEFINITION is a
    lambda-expression, it is coerced to a function. If DEFINITION is an
    interpreted function, it is compiled. If DEFINITION is already a compiled
    function, it is used as-is. (Future versions of SBCL might try to
    recompile the existing definition, but this is not currently supported.)
    
    If NAME is NIL, the compiled function is returned as the primary value.
    Otherwise the resulting compiled function replaces existing function
    definition of NAME, and NAME is returned as primary value; if NAME is a symbol
    that names a macro, its macro function is replaced and NAME is returned as
    primary value.
    
    Also returns a secondary value which is true if any conditions of type
    WARNING occur during the compilation, and NIL otherwise.
    
    Tertiary value is true if any conditions of type ERROR, or WARNING that are
    not STYLE-WARNINGs occur during compilation, and NIL otherwise.

  Inline proclamation: NOTINLINE (no inline expansion available)
  Known attributes: unwind, any
  Source file: SYS:SRC;COMPILER;TARGET-MAIN.LISP

```

### `COMPILE-FILE`

```lisp
COMMON-LISP:COMPILE-FILE
  [symbol]

COMPILE-FILE names a compiled function:
  Lambda-list: (INPUT-FILE &KEY (OUTPUT-FILE  OUTPUT-FILE-P)
                ((VERBOSE *COMPILE-VERBOSE*) *COMPILE-VERBOSE*)
                ((PRINT *COMPILE-PRINT*) *COMPILE-PRINT*)
                (EXTERNAL-FORMAT DEFAULT)
                ((PROGRESS *COMPILE-PROGRESS*) *COMPILE-PROGRESS*)
                (TRACE-FILE NIL)
                ((BLOCK-COMPILE *BLOCK-COMPILE-ARGUMENT*)
                 *BLOCK-COMPILE-DEFAULT*)
                ((ENTRY-POINTS *ENTRY-POINTS-ARGUMENT*) NIL)
                (EMIT-CFASL *EMIT-CFASL*))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:OUTPUT-FILE
                    (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                   (:VERBOSE T) (:PRINT T)
                   (:EXTERNAL-FORMAT (OR KEYWORD (CONS KEYWORD T)))
                   (:PROGRESS T) (:TRACE-FILE T) (:BLOCK-COMPILE T)
                   (:ENTRY-POINTS LIST) (:EMIT-CFASL T))
                  (VALUES (OR PATHNAME NULL) BOOLEAN BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                  &KEY (:OUTPUT-FILE #1#) (:VERBOSE . #2=(T))
                  (:PRINT . #2#)
                  (:EXTERNAL-FORMAT (OR KEYWORD (CONS KEYWORD . #2#)))
                  (:PROGRESS . #2#) (:TRACE-FILE . #2#)
                  (:BLOCK-COMPILE (MEMBER :SPECIFIED T NIL))
                  (:ENTRY-POINTS LIST) (:EMIT-CFASL . #2#))
                 (VALUES (OR PATHNAME NULL) BOOLEAN BOOLEAN &OPTIONAL))
  Documentation:
    Compile INPUT-FILE, producing a corresponding fasl file and
    returning its filename.
    
      :OUTPUT-FILE
         The name of the FASL to output, NIL for none, T for the default.
         (Note the difference between the treatment of NIL :OUTPUT-FILE
         here and in COMPILE-FILE-PATHNAME.)  The returned pathname of the
         output file may differ from the pathname of the :OUTPUT-FILE
         parameter, e.g. when the latter is a designator for a directory.
    
      :VERBOSE
         If true, information indicating what file is being compiled is printed
         to *STANDARD-OUTPUT*.
    
      :PRINT
         If true, each top level form in the file is printed to *STANDARD-OUTPUT*.
    
      :EXTERNAL-FORMAT
         The external format to use when opening the source file.
    
      :BLOCK-COMPILE {NIL | :SPECIFIED | T}
         Determines whether multiple functions are compiled together as a unit,
         resolving function references at compile time.  NIL means that global
         function names are never resolved at compilation time.  :SPECIFIED means
         that names are resolved at compile-time when convenient (as in a
         self-recursive call), but the compiler doesn't combine top-level DEFUNs.
         With :SPECIFIED, an explicit START-BLOCK declaration will enable block
         compilation.  A value of T indicates that all forms in the file(s) should
         be compiled as a unit.  The default is the value of
         SB-EXT:*BLOCK-COMPILE-DEFAULT*, which is initially NIL.
    
      :ENTRY-POINTS
         This specifies a list of function names for functions in the file(s) that
         must be given global definitions.  This only applies to block
         compilation, and is useful mainly when :BLOCK-COMPILE T is specified on a
         file that lacks START-BLOCK declarations.  If the value is NIL (the
         default) then all functions will be globally defined.
    
      :TRACE-FILE
         If given, internal data structures are dumped to the specified
         file, or if a value of T is given, to a file of *.trace type
         derived from the input file name. (non-standard)
    
      :EMIT-CFASL
         (Experimental). If true, outputs the toplevel compile-time effects
         of this file into a separate .cfasl file.
  Inline proclamation: NOTINLINE (no inline expansion available)
  Known attributes: unwind, any
  Source file: SYS:SRC;COMPILER;MAIN.LISP

```

### `COMPILE-FILE-PATHNAME`

```lisp
COMMON-LISP:COMPILE-FILE-PATHNAME
  [symbol]

COMPILE-FILE-PATHNAME names a compiled function:
  Lambda-list: (INPUT-FILE &KEY (OUTPUT-FILE NIL OUTPUT-FILE-P)
                &ALLOW-OTHER-KEYS)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:OUTPUT-FILE
                    (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM
                        BOOLEAN))
                   &ALLOW-OTHER-KEYS)
                  (VALUES PATHNAME &OPTIONAL))
  Documentation:
    Return a pathname describing what file COMPILE-FILE would write to given
       these arguments.
  Known attributes: unwind, any
  Source file: SYS:SRC;COMPILER;MAIN.LISP

```

### `COMPILED-FUNCTION-P`

```lisp
COMMON-LISP:COMPILED-FUNCTION-P
  [symbol]

COMPILED-FUNCTION-P names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a COMPILED-FUNCTION, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `COMPILER-MACRO-FUNCTION`

```lisp
COMMON-LISP:COMPILER-MACRO-FUNCTION
  [symbol]

COMPILER-MACRO-FUNCTION names a compiled function:
  Lambda-list: (NAME &OPTIONAL ENV)
  Declared type: (FUNCTION
                  (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES (OR NULL FUNCTION) &OPTIONAL))
  Documentation:
    If NAME names a compiler-macro in ENV, return the expansion function, else
    return NIL. Can be set with SETF when ENV is NIL.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;MACROEXPAND.LISP

(SETF COMPILER-MACRO-FUNCTION) names a compiled function:
  Lambda-list: (FUNCTION NAME &OPTIONAL ENV)
  Derived type: (FUNCTION
                 ((OR NULL FUNCTION) (OR CONS SYMBOL) &OPTIONAL T)
                 (VALUES (OR NULL FUNCTION) &OPTIONAL))
  Source file: SYS:SRC;CODE;MACROEXPAND.LISP

```

### `(SETF COMPILER-MACRO-FUNCTION)`

```lisp
COMMON-LISP-USER::|(SETF COMPILER-MACRO-FUNCTION)|
  [symbol]

```

### `COMPLEMENT`

```lisp
COMMON-LISP:COMPLEMENT
  [symbol]

COMPLEMENT names a compiled function:
  Lambda-list: (FUNCTION)
  Declared type: (FUNCTION (FUNCTION) (VALUES FUNCTION &OPTIONAL))
  Documentation:
    Return a new function that returns T whenever FUNCTION returns NIL and
       NIL whenever FUNCTION returns non-NIL.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FUNUTILS.LISP

```

### `COMPLEX`

```lisp
COMMON-LISP:COMPLEX
  [symbol]

COMPLEX names a compiled function:
  Lambda-list: (REALPART &OPTIONAL (IMAGPART 0))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR COMPLEX RATIONAL) &OPTIONAL))
  Documentation:
    Return a complex number with the specified real and imaginary components.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

COMPLEX names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:COMPLEX>:
  Class precedence-list: COMPLEX, NUMBER, T
  Direct superclasses: NUMBER
  Direct subclasses: SB-KERNEL:COMPLEX-DOUBLE-FLOAT,
                     SB-KERNEL:COMPLEX-SINGLE-FLOAT
  Sealed.
  No direct slots.

COMPLEX names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (TYPESPEC (QUOTE *)))

```

### `COMPLEXP`

```lisp
COMMON-LISP:COMPLEXP
  [symbol]

COMPLEXP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a COMPLEX, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `COMPUTE-RESTARTS`

```lisp
COMMON-LISP:COMPUTE-RESTARTS
  [symbol]

COMPUTE-RESTARTS names a compiled function:
  Lambda-list: (&OPTIONAL CONDITION)
  Declared type: (FUNCTION (&OPTIONAL (OR CONDITION NULL))
                  (VALUES LIST &OPTIONAL))
  Documentation:
    Return a list of all the currently active restarts ordered from most recently
    established to less recently established. If CONDITION is specified, then only
    restarts associated with CONDITION (or with no condition) will be returned.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `CONCATENATE`

```lisp
COMMON-LISP:CONCATENATE
  [symbol]

CONCATENATE names a compiled function:
  Lambda-list: (RESULT-TYPE &REST SEQUENCES)
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS) &REST
                   SEQUENCE)
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION (T &REST T)
                 (VALUES
                  (OR LIST (SIMPLE-ARRAY * (*))
                      SB-KERNEL:EXTENDED-SEQUENCE)
                  &OPTIONAL))
  Documentation:
    Return a new sequence of all the argument sequences concatenated together
      which shares no structure with the original argument sequences of the
      specified RESULT-TYPE.
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `CONCATENATED-STREAM-STREAMS`

```lisp
COMMON-LISP:CONCATENATED-STREAM-STREAMS
  [symbol]

CONCATENATED-STREAM-STREAMS names a compiled function:
  Lambda-list: (STREAM)
  Derived type: (FUNCTION (T) (VALUES LIST &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `CONJUGATE`

```lisp
COMMON-LISP:CONJUGATE
  [symbol]

CONJUGATE names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Return the complex conjugate of NUMBER. For non-complex numbers, this is
      an identity.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `CONS`

```lisp
COMMON-LISP:CONS
  [symbol]

CONS names a compiled function:
  Lambda-list: (SE1 SE2)
  Declared type: (FUNCTION (T T) (VALUES CONS &OPTIONAL))
  Documentation:
    Return a list with SE1 as the CAR and SE2 as the CDR.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;LIST.LISP

CONS names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:CONS>:
  Class precedence-list: CONS, LIST, SEQUENCE, T
  Direct superclasses: LIST
  No subclasses.
  Sealed.
  No direct slots.

CONS names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (CAR-TYPE-SPEC (QUOTE *))
                (CDR-TYPE-SPEC (QUOTE *)))

```

### `CONSP`

```lisp
COMMON-LISP:CONSP
  [symbol]

CONSP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a CONS, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `CONSTANTLY`

```lisp
COMMON-LISP:CONSTANTLY
  [symbol]

CONSTANTLY names a compiled function:
  Lambda-list: (VALUE)
  Declared type: (FUNCTION (T) (VALUES FUNCTION &OPTIONAL))
  Documentation:
    Return a function that always returns VALUE.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FUNUTILS.LISP

```

### `CONSTANTP`

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

### `CONTINUE`

```lisp
COMMON-LISP:CONTINUE
  [symbol]

CONTINUE names a compiled function:
  Lambda-list: (&OPTIONAL CONDITION)
  Declared type: (FUNCTION (&OPTIONAL (OR CONDITION NULL))
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Transfer control to a restart named CONTINUE, or return NIL if none exists.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `COPY-ALIST`

```lisp
COMMON-LISP:COPY-ALIST
  [symbol]

COPY-ALIST names a compiled function:
  Lambda-list: (ALIST)
  Declared type: (FUNCTION (LIST) (VALUES LIST &OPTIONAL))
  Documentation:
    Return a new association list which is EQUAL to ALIST.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `COPY-LIST`

```lisp
COMMON-LISP:COPY-LIST
  [symbol]

COPY-LIST names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES LIST &OPTIONAL))
  Documentation:
    Return a new list which is EQUAL to LIST. LIST may be improper.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `COPY-PPRINT-DISPATCH`

```lisp
COMMON-LISP:COPY-PPRINT-DISPATCH
  [symbol]

COPY-PPRINT-DISPATCH names a compiled function:
  Lambda-list: (&OPTIONAL (TABLE *PRINT-PPRINT-DISPATCH*))
  Declared type: (FUNCTION
                  (&OPTIONAL (OR SB-PRETTY:PPRINT-DISPATCH-TABLE NULL))
                  (VALUES SB-PRETTY:PPRINT-DISPATCH-TABLE &OPTIONAL))
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `COPY-READTABLE`

```lisp
COMMON-LISP:COPY-READTABLE
  [symbol]

COPY-READTABLE names a compiled function:
  Lambda-list: (&OPTIONAL (FROM-READTABLE *READTABLE*) TO-READTABLE)
  Declared type: (FUNCTION
                  (&OPTIONAL (OR READTABLE NULL) (OR READTABLE NULL))
                  (VALUES READTABLE &OPTIONAL))
  Documentation:
    Copies FROM-READTABLE and returns the result. Uses TO-READTABLE as a target
    for the copy when provided, otherwise a new readtable is created. The
    FROM-READTABLE defaults to the standard readtable when NIL and to the current
    readtable when not provided.
  Source file: SYS:SRC;CODE;READER.LISP

```

### `COPY-SEQ`

```lisp
COMMON-LISP:COPY-SEQ
  [symbol]

COPY-SEQ names a compiled function:
  Lambda-list: (SEQUENCE)
  Declared type: (FUNCTION (SEQUENCE)
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return a copy of SEQUENCE which is EQUAL to SEQUENCE but not EQ.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `COPY-STRUCTURE`

```lisp
COMMON-LISP:COPY-STRUCTURE
  [symbol]

COPY-STRUCTURE names a compiled function:
  Lambda-list: (STRUCTURE)
  Declared type: (FUNCTION (STRUCTURE-OBJECT)
                  (VALUES STRUCTURE-OBJECT &OPTIONAL))
  Documentation:
    Return a copy of STRUCTURE with the same (EQL) slot values.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-DEFSTRUCT.LISP

```

### `COPY-SYMBOL`

```lisp
COMMON-LISP:COPY-SYMBOL
  [symbol]

COPY-SYMBOL names a compiled function:
  Lambda-list: (SYMBOL &OPTIONAL (COPY-PROPS NIL))
  Declared type: (FUNCTION (SYMBOL &OPTIONAL T)
                  (VALUES SYMBOL &OPTIONAL))
  Documentation:
    Make and return a new uninterned symbol with the same print name
      as SYMBOL. If COPY-PROPS is false, the new symbol is neither bound
      nor fbound and has no properties, else it has a copy of SYMBOL's
      function, value and property list.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `COPY-TREE`

```lisp
COMMON-LISP:COPY-TREE
  [symbol]

COPY-TREE names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES T &OPTIONAL))
  Documentation:
    Recursively copy trees of conses.
  Known attributes: flushable, unsafely-flushable, recursive
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `COS`

```lisp
COMMON-LISP:COS
  [symbol]

COS names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR (FLOAT -1.0 1.0) (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (FLOAT -1.0 1.0) (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    Return the cosine of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `COSH`

```lisp
COMMON-LISP:COSH
  [symbol]

COSH names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (FLOAT 1.0) (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    Return the hyperbolic cosine of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `COUNT`

```lisp
COMMON-LISP:COUNT
  [symbol]

COUNT names a compiled function:
  Lambda-list: (ITEM SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                (END NIL) (KEY NIL) (TEST (FUNCTION EQL) TEST-P)
                (TEST-NOT NIL TEST-NOT-P))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T SEQUENCE &REST T &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))) (:FROM-END T)
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &REST T &KEY (:TEST . #1=((OR FUNCTION SYMBOL)))
                  (:TEST-NOT . #1#)
                  (:START . #2=(#3=(UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:FROM-END T) (:KEY . #1#))
                 (VALUES #3# &OPTIONAL))
  Documentation:
    Return the number of elements in SEQUENCE satisfying a test with ITEM,
       which defaults to EQL.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `COUNT-IF`

```lisp
COMMON-LISP:COUNT-IF
  [symbol]

COUNT-IF names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                (END NIL) (KEY NIL))
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:START . #2=(#3=(UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES #3# &OPTIONAL))
  Documentation:
    Return the number of elements in SEQUENCE satisfying PRED(el).
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `COUNT-IF-NOT`

```lisp
COMMON-LISP:COUNT-IF-NOT
  [symbol]

COUNT-IF-NOT names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                (END NIL) (KEY NIL))
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:START . #2=(#3=(UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES #3# &OPTIONAL))
  Documentation:
    Return the number of elements in SEQUENCE not satisfying TEST(el).
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `DECODE-FLOAT`

```lisp
COMMON-LISP:DECODE-FLOAT
  [symbol]

DECODE-FLOAT names a compiled function:
  Lambda-list: (F)
  Declared type: (FUNCTION (FLOAT)
                  (VALUES FLOAT (INTEGER -1074 1024) FLOAT &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES FLOAT (INTEGER -1074 1024)
                         (OR (FLOAT 1.0 1.0) (SINGLE-FLOAT -1.0 -1.0)
                             (DOUBLE-FLOAT -1.0d0 -1.0d0))
                         &OPTIONAL))
  Documentation:
    Return three values:
       1) a floating-point number representing the significand. This is always
          between 0.5 (inclusive) and 1.0 (exclusive).
       2) an integer representing the exponent.
       3) -1.0 or 1.0 (i.e. the sign of the argument.)
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

```

### `DECODE-UNIVERSAL-TIME`

```lisp
COMMON-LISP:DECODE-UNIVERSAL-TIME
  [symbol]

DECODE-UNIVERSAL-TIME names a compiled function:
  Lambda-list: (UNIVERSAL-TIME &OPTIONAL TIME-ZONE)
  Declared type: (FUNCTION
                  (UNSIGNED-BYTE &OPTIONAL (OR NULL (RATIONAL -24 24)))
                  (VALUES (MOD 60) (MOD 60) (MOD 24) (INTEGER 1 31)
                          (INTEGER 1 12) UNSIGNED-BYTE (MOD 7) BOOLEAN
                          (RATIONAL -24 24) &OPTIONAL))
  Derived type: (FUNCTION
                 (UNSIGNED-BYTE &OPTIONAL
                  (OR NULL #1=(RATIONAL -24 . #2=(24))))
                 (VALUES #3=(MOD 60) #3# (MOD . #2#) (INTEGER 1 31)
                         (INTEGER 1 12) (INTEGER 1800) (MOD 7) BOOLEAN
                         #1# &OPTIONAL))
  Documentation:
    Converts a universal-time to decoded time format returning the following
       nine values: second, minute, hour, date, month, year, day of week (0 =
       Monday), T (daylight savings time) or NIL (standard time), and timezone.
       Completely ignores daylight-savings-time when time-zone is supplied.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TIME.LISP

```

### `DELETE`

```lisp
COMMON-LISP:DELETE
  [symbol]

DELETE names a compiled function:
  Lambda-list: (ITEM SEQUENCE &REST ARGS &KEY FROM-END
                (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START 0)
                (END NIL) (COUNT NIL) (KEY NIL))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT :KEY)
  Declared type: (FUNCTION
                  (T SEQUENCE &REST T &KEY (:FROM-END T)
                   (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:COUNT (OR NULL INTEGER))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &REST T &KEY (:FROM-END T)
                  (:TEST . #1=((OR FUNCTION SYMBOL))) (:TEST-NOT . #1#)
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:COUNT (OR NULL INTEGER))
                  (:KEY . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a sequence formed by destructively removing the specified ITEM from
      the given SEQUENCE.
  Known attributes: call, important-result
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `DELETE-DUPLICATES`

```lisp
COMMON-LISP:DELETE-DUPLICATES
  [symbol]

DELETE-DUPLICATES names a compiled function:
  Lambda-list: (SEQUENCE &REST ARGS &KEY (TEST (FUNCTION EQL))
                (TEST-NOT NIL) (START 0) (END NIL) FROM-END (KEY NIL))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT :KEY)
  Declared type: (FUNCTION
                  (SEQUENCE &REST T &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))) (:FROM-END T)
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (T &REST T &KEY (:TEST . #1=((OR FUNCTION SYMBOL)))
                  (:TEST-NOT . #1#) (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:FROM-END T) (:KEY . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    The elements of SEQUENCE are examined, and if any two match, one is
       discarded. The resulting sequence, which may be formed by destroying the
       given sequence, is returned.
    
       The :TEST-NOT argument is deprecated.
  Known attributes: call, important-result
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `DELETE-FILE`

```lisp
COMMON-LISP:DELETE-FILE
  [symbol]

DELETE-FILE names a compiled function:
  Lambda-list: (FILE)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Delete the specified FILE.
    
    If FILE is a stream, on Windows the stream is closed immediately. On Unix
    platforms the stream remains open, allowing IO to continue: the OS resources
    associated with the deleted file remain available till the stream is closed as
    per standard Unix unlink() behaviour.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `DELETE-IF`

```lisp
COMMON-LISP:DELETE-IF
  [symbol]

DELETE-IF names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                (KEY NIL) (END NIL) (COUNT NIL))
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:COUNT (OR NULL INTEGER))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:COUNT (OR NULL INTEGER))
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a sequence formed by destructively removing the elements satisfying
      the specified PREDICATE from the given SEQUENCE.
  Known attributes: call, important-result
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `DELETE-IF-NOT`

```lisp
COMMON-LISP:DELETE-IF-NOT
  [symbol]

DELETE-IF-NOT names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                (END NIL) (KEY NIL) (COUNT NIL))
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:COUNT (OR NULL INTEGER))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:COUNT (OR NULL INTEGER))
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a sequence formed by destructively removing the elements not
      satisfying the specified PREDICATE from the given SEQUENCE.
  Known attributes: call, important-result
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `DELETE-PACKAGE`

```lisp
COMMON-LISP:DELETE-PACKAGE
  [symbol]

DELETE-PACKAGE names a compiled function:
  Lambda-list: (PACKAGE-DESIGNATOR)
  Derived type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Delete the package designated by PACKAGE-DESIGNATOR from the package
      system data structures.
  Source file: SYS:SRC;CODE;DEFPACKAGE.LISP

```

### `DENOMINATOR`

```lisp
COMMON-LISP:DENOMINATOR
  [symbol]

DENOMINATOR names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (RATIONAL) (VALUES (INTEGER 1) &OPTIONAL))
  Documentation:
    Return the denominator of NUMBER, which must be rational.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `DEPOSIT-FIELD`

```lisp
COMMON-LISP:DEPOSIT-FIELD
  [symbol]

DEPOSIT-FIELD names a compiled function:
  Lambda-list: (NEWBYTE BYTESPEC INTEGER)
  Declared type: (FUNCTION (INTEGER CONS INTEGER)
                  (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return new integer with newbyte in specified position, newbyte is not right justified.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `DESCRIBE`

```lisp
COMMON-LISP:DESCRIBE
  [symbol]

DESCRIBE names a compiled function:
  Lambda-list: (OBJECT &OPTIONAL (STREAM-DESIGNATOR *STANDARD-OUTPUT*))
  Declared type: (FUNCTION (T &OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES &OPTIONAL))
  Documentation:
    Print a description of OBJECT to STREAM-DESIGNATOR.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;DESCRIBE.LISP

```

### `DIGIT-CHAR`

```lisp
COMMON-LISP:DIGIT-CHAR
  [symbol]

DIGIT-CHAR names a compiled function:
  Lambda-list: (WEIGHT &OPTIONAL (RADIX 10))
  Declared type: (FUNCTION (UNSIGNED-BYTE &OPTIONAL (INTEGER 2 36))
                  (VALUES (OR CHARACTER NULL) &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL (INTEGER 2 36))
                 (VALUES
                  (OR (SB-KERNEL:CHARACTER-SET ((48 . 57) (65 . 90)))
                      NULL)
                  &OPTIONAL))
  Documentation:
    All arguments must be integers. Returns a character object that represents
    a digit of the given weight in the specified radix. Returns NIL if no such
    character exists.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `DIGIT-CHAR-P`

```lisp
COMMON-LISP:DIGIT-CHAR-P
  [symbol]

DIGIT-CHAR-P names a compiled function:
  Lambda-list: (CHAR &OPTIONAL (RADIX 10))
  Declared type: (FUNCTION (CHARACTER &OPTIONAL (INTEGER 2 36))
                  (VALUES (OR (MOD 36) NULL) &OPTIONAL))
  Documentation:
    If char is a digit in the specified radix, returns the fixnum for which
    that digit stands, else returns NIL.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `DIRECTORY`

```lisp
COMMON-LISP:DIRECTORY
  [symbol]

DIRECTORY names a compiled function:
  Lambda-list: (PATHSPEC &KEY (RESOLVE-SYMLINKS T))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:RESOLVE-SYMLINKS T))
                  (VALUES LIST &OPTIONAL))
  Documentation:
    Return a list of PATHNAMEs, each the TRUENAME of a file matching PATHSPEC.
    
    Note that the interaction between this ANSI-specified TRUENAMEing and
    the semantics of the Unix filesystem (symbolic links..) means this
    function can sometimes return files which don't have the same
    directory as PATHSPEC.
    
    If :RESOLVE-SYMLINKS is NIL, don't resolve symbolic links in matching
    filenames.
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `DIRECTORY-NAMESTRING`

```lisp
COMMON-LISP:DIRECTORY-NAMESTRING
  [symbol]

DIRECTORY-NAMESTRING names a compiled function:
  Lambda-list: (PATHNAME)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Return a string representation of the directory in PATHNAME.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `DISASSEMBLE`

```lisp
COMMON-LISP:DISASSEMBLE
  [symbol]

DISASSEMBLE names a compiled function:
  Lambda-list: (OBJECT &KEY (STREAM *STANDARD-OUTPUT*) (USE-LABELS T))
  Declared type: (FUNCTION
                  ((OR
                    (SATISFIES SB-INT:EXTENDED-FUNCTION-DESIGNATOR-P)
                    (CONS (MEMBER LAMBDA) T) SB-KERNEL:CODE-COMPONENT)
                   &KEY (:STREAM STREAM) (:USE-LABELS T))
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Disassemble the compiled code associated with OBJECT, which can be a
      function, a lambda expression, or a symbol with a function definition. If
      it is not already compiled, the compiler is called to produce something to
      disassemble.
  Known attributes: unwind, any
  Source file: SYS:SRC;COMPILER;TARGET-DISASSEM.LISP

```

### `DPB`

```lisp
COMMON-LISP:DPB
  [symbol]

DPB names a compiled function:
  Lambda-list: (NEWBYTE BYTESPEC INTEGER)
  Declared type: (FUNCTION (INTEGER CONS INTEGER)
                  (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return new integer with newbyte in specified position, newbyte is right justified.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `DRIBBLE`

```lisp
COMMON-LISP:DRIBBLE
  [symbol]

DRIBBLE names a compiled function:
  Lambda-list: (&OPTIONAL PATHNAME &KEY (IF-EXISTS APPEND))
  Declared type: (FUNCTION
                  (&OPTIONAL (OR STRING PATHNAME) &KEY (:IF-EXISTS T))
                  (VALUES &OPTIONAL))
  Derived type: (FUNCTION
                 (&OPTIONAL (OR STRING PATHNAME NULL) &KEY
                  (:IF-EXISTS T))
                 (VALUES &OPTIONAL))
  Documentation:
    With a file name as an argument, dribble opens the file and sends a
      record of further I/O to that file. Without an argument, it closes
      the dribble file, and quits logging.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-MISC.LISP

```

### `ECHO-STREAM-INPUT-STREAM`

```lisp
COMMON-LISP:ECHO-STREAM-INPUT-STREAM
  [symbol]

ECHO-STREAM-INPUT-STREAM names a compiled function:
  Lambda-list: (INSTANCE)
  Derived type: (FUNCTION (ECHO-STREAM) (VALUES STREAM &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `ECHO-STREAM-OUTPUT-STREAM`

```lisp
COMMON-LISP:ECHO-STREAM-OUTPUT-STREAM
  [symbol]

ECHO-STREAM-OUTPUT-STREAM names a compiled function:
  Lambda-list: (INSTANCE)
  Derived type: (FUNCTION (ECHO-STREAM) (VALUES STREAM &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `ED`

```lisp
COMMON-LISP:ED
  [symbol]

ED names a compiled function:
  Lambda-list: (&OPTIONAL X)
  Declared type: (FUNCTION (&OPTIONAL (OR STRING SYMBOL CONS PATHNAME))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL (OR STRING SYMBOL CONS PATHNAME))
                 (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Starts the editor (on a file or a function if named).  Functions
    from the list *ED-FUNCTIONS* are called in order with X as an argument
    until one of them returns non-NIL; these functions are responsible for
    signalling a FILE-ERROR to indicate failure to perform an operation on
    the file system.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-MISC.LISP

```

### `EIGHTH`

```lisp
COMMON-LISP:EIGHTH
  [symbol]

EIGHTH names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 8th object in a list or NIL if there is no 8th object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF EIGHTH) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF EIGHTH) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF EIGHTH)`

```lisp
COMMON-LISP-USER::|(SETF EIGHTH)|
  [symbol]

```

### `ELT`

```lisp
COMMON-LISP:ELT
  [symbol]

ELT names a compiled function:
  Lambda-list: (SEQUENCE INDEX)
  Declared type: (FUNCTION (SEQUENCE (UNSIGNED-BYTE 45))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T (UNSIGNED-BYTE 45)) (VALUES T &OPTIONAL))
  Documentation:
    Return the element of SEQUENCE specified by INDEX.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

(SETF ELT) names a compiled function:
  Lambda-list: (NEWVAL SEQUENCE INDEX)
  Derived type: (FUNCTION (T SEQUENCE (UNSIGNED-BYTE 45))
                 (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF ELT) has setf-expansion: SB-KERNEL:%SETELT

```

### `(SETF ELT)`

```lisp
COMMON-LISP-USER::|(SETF ELT)|
  [symbol]

```

### `ENCODE-UNIVERSAL-TIME`

```lisp
COMMON-LISP:ENCODE-UNIVERSAL-TIME
  [symbol]

ENCODE-UNIVERSAL-TIME names a compiled function:
  Lambda-list: (SECOND MINUTE HOUR DATE MONTH YEAR &OPTIONAL TIME-ZONE)
  Declared type: (FUNCTION
                  ((MOD 60) (MOD 60) (MOD 24) (INTEGER 1 31)
                   (INTEGER 1 12) UNSIGNED-BYTE &OPTIONAL
                   (OR NULL (RATIONAL -24 24)))
                  (VALUES UNSIGNED-BYTE &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(MOD 60) #1# (MOD . #2=(24)) (INTEGER 1 31)
                  (INTEGER 1 12) (OR (MOD 100) (INTEGER 1899))
                  &OPTIONAL (OR NULL (RATIONAL -24 . #2#)))
                 (VALUES UNSIGNED-BYTE &OPTIONAL))
  Documentation:
    The time values specified in decoded format are converted to
       universal time, which is returned.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TIME.LISP

```

### `ENDP`

```lisp
COMMON-LISP:ENDP
  [symbol]

ENDP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (LIST) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    This is the recommended way to test for the end of a proper list. It
       returns true if OBJECT is NIL, false if OBJECT is a CONS, and an error
       for any other type of OBJECT.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `ENOUGH-NAMESTRING`

```lisp
COMMON-LISP:ENOUGH-NAMESTRING
  [symbol]

ENOUGH-NAMESTRING names a compiled function:
  Lambda-list: (PATHNAME &OPTIONAL
                         (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   &OPTIONAL
                   (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Return an abbreviated pathname sufficient to identify PATHNAME
    relative to DEFAULTS.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `ENSURE-DIRECTORIES-EXIST`

```lisp
COMMON-LISP:ENSURE-DIRECTORIES-EXIST
  [symbol]

ENSURE-DIRECTORIES-EXIST names a compiled function:
  Lambda-list: (PATHSPEC &KEY VERBOSE (MODE 511))
  Derived type: (FUNCTION (T &KEY (:VERBOSE T) (:MODE T))
                 (VALUES T BOOLEAN &OPTIONAL))
  Documentation:
    Test whether the directories containing the specified file
      actually exist, and attempt to create them if they do not.
      The MODE argument is a CMUCL/SBCL-specific extension to control
      the Unix permission bits.
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `ENSURE-GENERIC-FUNCTION`

```lisp
COMMON-LISP:ENSURE-GENERIC-FUNCTION
  [symbol]

ENSURE-GENERIC-FUNCTION names a compiled function:
  Lambda-list: (FUN-NAME &REST ALL-KEYS)
  Derived type: (FUNCTION (T &REST T) *)
  Source file: SYS:SRC;PCL;BOOT.LISP

```

### `EQ`

```lisp
COMMON-LISP:EQ
  [symbol]

EQ names a compiled function:
  Lambda-list: (OBJ1 OBJ2)
  Declared type: (FUNCTION (T T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if OBJ1 and OBJ2 are the same object, otherwise NIL.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate, commutative
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `EQL`

```lisp
COMMON-LISP:EQL
  [symbol]

EQL names a compiled function:
  Lambda-list: (X Y)
  Declared type: (FUNCTION (T T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate, commutative
  Source file: SYS:SRC;CODE;PRED.LISP

EQL names a type-specifier:
  Lambda-list: (N)

```

### `EQUAL`

```lisp
COMMON-LISP:EQUAL
  [symbol]

EQUAL names a compiled function:
  Lambda-list: (X Y)
  Declared type: (FUNCTION (T T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if X and Y are EQL or if they are structured components whose
    elements are EQUAL. Strings and bit-vectors are EQUAL if they are the same
    length and have identical components. Other arrays must be EQ to be EQUAL.
  Known attributes: foldable, flushable, unsafely-flushable, recursive
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `EQUALP`

```lisp
COMMON-LISP:EQUALP
  [symbol]

EQUALP names a compiled function:
  Lambda-list: (X Y)
  Declared type: (FUNCTION (T T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Just like EQUAL, but more liberal in several respects.
      Numbers may be of different types, as long as the values are identical
      after coercion. Characters may differ in alphabetic case. Vectors and
      arrays must have identical dimensions and EQUALP elements, but may differ
      in their type restriction. The elements of structured components
      are compared with EQUALP.
  Known attributes: foldable, flushable, unsafely-flushable, recursive
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `ERROR`

```lisp
COMMON-LISP:ERROR
  [symbol]

ERROR names a compiled function:
  Lambda-list: (DATUM &REST ARGUMENTS)
  Declared type: (FUNCTION
                  ((OR STRING FUNCTION SYMBOL CONDITION
                       SB-PCL::CONDITION-CLASS)
                   &REST T)
                  NIL)
  Documentation:
    Invoke the signal facility on a condition formed from DATUM and ARGUMENTS.
      If the condition is not handled, the debugger is invoked.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;COLD-ERROR.LISP

ERROR names the condition-class #<SB-PCL::CONDITION-CLASS COMMON-LISP:ERROR>:
  Class precedence-list: ERROR, SERIOUS-CONDITION, CONDITION,
                         SB-PCL::SLOT-OBJECT, T
  Direct superclasses: SERIOUS-CONDITION
  Direct subclasses: ALIVE/LSP/ERRORS:SERVER-ERROR,
                     ALIVE/LSP/ERRORS:UNHANDLED-REQUEST,
                     ALIVE/THREADS:THREAD-NOT-FOUND,
                     ALIVE/PACKAGES:PACKAGE-NOT-FOUND,
                     ALIVE/ERRORS:INPUT-ERROR,
                     FLEXI-STREAMS:EXTERNAL-FORMAT-ERROR,
                     BORDEAUX-THREADS-2:BORDEAUX-THREADS-ERROR,
                     BORDEAUX-THREADS::BORDEAUX-MP-CONDITION,
                     JSON-RPC:JSON-RPC-CALL-ERROR,
                     JSON:NO-CHAR-FOR-CODE, USOCKET:NS-ERROR,
                     USOCKET:SOCKET-ERROR,
                     USOCKET:INSUFFICIENT-IMPLEMENTATION,
                     QL-BUNDLE::BUNDLE-ERROR,
                     QUICKLISP-CLIENT::INVALID-CLIENT-INFO,
                     QUICKLISP-CLIENT::INVALID-CLIENT-FILE,
                     QUICKLISP-CLIENT::SYSTEM-NOT-QUICKLOADABLE,
                     QUICKLISP-CLIENT:SYSTEM-NOT-FOUND,
                     QL-DIST:UNKNOWN-DIST,
                     QL-DIST:INVALID-LOCAL-ARCHIVE,
                     QL-DIST:SUBSCRIPTION-UNAVAILABLE,
                     QL-HTTP:FETCH-ERROR, QL-HTTP::END-OF-DATA,
                     QL-HTTP::MATCH-FAILURE,
                     SB-BSD-SOCKETS:NAME-SERVICE-ERROR,
                     SB-BSD-SOCKETS:UNKNOWN-PROTOCOL,
                     SB-BSD-SOCKETS:SOCKET-ERROR,
                     SB-POSIX:SYSCALL-ERROR,
                     ASDF/BACKWARD-INTERFACE:OPERATION-ERROR,
                     ASDF/SESSION:SYSTEM-DEFINITION-ERROR,
                     UIOP/RUN-PROGRAM:SUBPROCESS-ERROR,
                     UIOP/LISP-BUILD:COMPILE-FAILED-ERROR,
                     UIOP/LISP-BUILD:COMPILE-WARNED-ERROR,
                     UIOP/LISP-BUILD:COMPILE-FILE-ERROR,
                     UIOP/VERSION:DEPRECATED-FUNCTION-SHOULD-BE-DELETED,
                     UIOP/VERSION:DEPRECATED-FUNCTION-ERROR,
                     UIOP/UTILITY:PARAMETER-ERROR,
                     UIOP/UTILITY:NOT-IMPLEMENTED-ERROR,
                     SB-IMPL::SAVE-ERROR,
                     SB-PCL::NEW-VALUE-SPECIALIZATION,
                     SB-PCL::OBSOLETE-STRUCTURE,
                     SB-PCL::CPL-PROTOCOL-VIOLATION,
                     SB-PCL::INVALID-SUPERCLASS,
                     SB-PCL::SLOTD-INITIALIZATION-ERROR,
                     SB-PCL::EFFECTIVE-METHOD-ERROR,
                     SB-PCL::INSTANCE-STRUCTURE-PROTOCOL-ERROR,
                     SB-PCL:SPECIALIZER-NAME-SYNTAX-ERROR,
                     SB-SYS:MEMORY-FAULT-ERROR, SB-DI:DEBUG-ERROR,
                     SB-THREAD:THREAD-ERROR,
                     SB-SYS:FOREIGN-HEAP-CORRUPTION, SB-WIN32:EXCEPTION,
                     SB-KERNEL::DEFMACRO-LAMBDA-LIST-BIND-ERROR,
                     SB-FORMAT:FORMAT-ERROR, INVALID-FASL,
                     SB-SYS:BREAKPOINT-ERROR,
                     SB-INT:CHARACTER-CODING-ERROR, DEPRECATION-ERROR,
                     SB-KERNEL:FTYPE-PROCLAMATION-MISMATCH-ERROR,
                     SB-INT:STANDARD-PPRINT-DISPATCH-TABLE-MODIFIED-ERROR,
                     SB-INT:STANDARD-READTABLE-MODIFIED-ERROR,
                     DEFCONSTANT-UNEQL, PRINT-NOT-READABLE,
                     SB-KERNEL::ILLEGAL-CLASS-NAME-ERROR,
                     ARITHMETIC-ERROR, CELL-ERROR, PACKAGE-ERROR,
                     FILE-ERROR, STREAM-ERROR, CONTROL-ERROR,
                     PARSE-ERROR, PROGRAM-ERROR, TYPE-ERROR,
                     SIMPLE-ERROR
  No direct slots.

```

### `EVAL`

```lisp
COMMON-LISP:EVAL
  [symbol]

EVAL names a compiled function:
  Lambda-list: (ORIGINAL-EXP)
  Declared type: (FUNCTION (T) *)
  Documentation:
    Evaluate the argument in a null lexical environment, returning the
       result or results.
  Known attributes: recursive
  Source file: SYS:SRC;CODE;EVAL.LISP

```

### `EVENP`

```lisp
COMMON-LISP:EVENP
  [symbol]

EVENP names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (INTEGER) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Is this integer even?
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `EVERY`

```lisp
COMMON-LISP:EVERY
  [symbol]

EVERY names a compiled function:
  Lambda-list: (PRED FIRST-SEQ &REST MORE-SEQS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST SEQUENCE)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION ((OR FUNCTION SYMBOL) SEQUENCE &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Apply PREDICATE to the 0-indexed elements of the sequences, then
       possibly to those with index 1, and so on. Return NIL as soon
       as any invocation of PREDICATE returns NIL, or T if every invocation
       is non-NIL.
  Known attributes: call, foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;QUANTIFIERS.LISP

```

### `EXP`

```lisp
COMMON-LISP:EXP
  [symbol]

EXP names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (FLOAT 0.0) (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    Return e raised to the power NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `EXPORT`

```lisp
COMMON-LISP:EXPORT
  [symbol]

EXPORT names a compiled function:
  Lambda-list: (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Exports SYMBOLS from PACKAGE, checking that no name conflicts result.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `EXPT`

```lisp
COMMON-LISP:EXPT
  [symbol]

EXPT names a compiled function:
  Lambda-list: (BASE POWER)
  Declared type: (FUNCTION (NUMBER NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Return BASE raised to the POWER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `FBOUNDP`

```lisp
COMMON-LISP:FBOUNDP
  [symbol]

FBOUNDP names a compiled function:
  Lambda-list: (NAME)
  Declared type: (FUNCTION ((OR CONS SYMBOL))
                  (VALUES (OR NULL FUNCTION) &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (OR NULL FUNCTION) &OPTIONAL))
  Documentation:
    Return true if name has a global function definition.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;FDEFINITION.LISP

```

### `FCEILING`

```lisp
COMMON-LISP:FCEILING
  [symbol]

FCEILING names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES FLOAT REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES FLOAT REAL &OPTIONAL))
  Documentation:
    Same as CEILING, but returns first value as a float.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `FDEFINITION`

```lisp
COMMON-LISP:FDEFINITION
  [symbol]

FDEFINITION names a compiled function:
  Lambda-list: (NAME)
  Declared type: (FUNCTION ((OR CONS SYMBOL))
                  (VALUES FUNCTION &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return name's global function definition taking care to respect any
       encapsulations and to return the innermost encapsulated definition.
       This is SETF'able.
  Source file: SYS:SRC;CODE;FDEFINITION.LISP

(SETF FDEFINITION) names a compiled function:
  Lambda-list: (NEW-VALUE NAME)
  Declared type: (FUNCTION (FUNCTION (OR CONS SYMBOL))
                  (VALUES FUNCTION &OPTIONAL))
  Derived type: (FUNCTION (FUNCTION T) (VALUES FUNCTION &OPTIONAL))
  Documentation:
    Set NAME's global function definition.
  Source file: SYS:SRC;CODE;FDEFINITION.LISP

```

### `(SETF FDEFINITION)`

```lisp
COMMON-LISP-USER::|(SETF FDEFINITION)|
  [symbol]

```

### `FFLOOR`

```lisp
COMMON-LISP:FFLOOR
  [symbol]

FFLOOR names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES FLOAT REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES FLOAT REAL &OPTIONAL))
  Documentation:
    Same as FLOOR, but returns first value as a float.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `FIFTH`

```lisp
COMMON-LISP:FIFTH
  [symbol]

FIFTH names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 5th object in a list or NIL if there is no 5th object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF FIFTH) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF FIFTH) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF FIFTH)`

```lisp
COMMON-LISP-USER::|(SETF FIFTH)|
  [symbol]

```

### `FILE-AUTHOR`

```lisp
COMMON-LISP:FILE-AUTHOR
  [symbol]

FILE-AUTHOR names a compiled function:
  Lambda-list: (PATHSPEC)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Return the author of the file specified by PATHSPEC. Signal an
    error of type FILE-ERROR if no such file exists, or if PATHSPEC
    is a wild pathname.
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `FILE-ERROR-PATHNAME`

```lisp
COMMON-LISP:FILE-ERROR-PATHNAME
  [symbol]

FILE-ERROR-PATHNAME names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `FILE-LENGTH`

```lisp
COMMON-LISP:FILE-LENGTH
  [symbol]

FILE-LENGTH names a compiled function:
  Lambda-list: (STREAM)
  Declared type: (FUNCTION
                  ((OR FILE-STREAM SYNONYM-STREAM BROADCAST-STREAM))
                  (VALUES (OR UNSIGNED-BYTE NULL) &OPTIONAL))
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `FILE-NAMESTRING`

```lisp
COMMON-LISP:FILE-NAMESTRING
  [symbol]

FILE-NAMESTRING names a compiled function:
  Lambda-list: (PATHNAME)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Return a string representation of the name in PATHNAME.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `FILE-POSITION`

```lisp
COMMON-LISP:FILE-POSITION
  [symbol]

FILE-POSITION names a compiled function:
  Lambda-list: (STREAM &OPTIONAL (POSITION 0 SUPPLIEDP))
  Declared type: (FUNCTION
                  (STREAM &OPTIONAL
                   (OR UNSIGNED-BYTE (MEMBER :END :START)))
                  (VALUES (OR UNSIGNED-BYTE BOOLEAN) &OPTIONAL))
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `FILE-STRING-LENGTH`

```lisp
COMMON-LISP:FILE-STRING-LENGTH
  [symbol]

FILE-STRING-LENGTH names a compiled function:
  Lambda-list: (STREAM OBJECT)
  Declared type: (FUNCTION (STREAM (OR STRING CHARACTER))
                  (VALUES (OR UNSIGNED-BYTE NULL) &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `FILE-WRITE-DATE`

```lisp
COMMON-LISP:FILE-WRITE-DATE
  [symbol]

FILE-WRITE-DATE names a compiled function:
  Lambda-list: (PATHSPEC)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES (OR UNSIGNED-BYTE NULL) &OPTIONAL))
  Documentation:
    Return the write date of the file specified by PATHSPEC.
    An error of type FILE-ERROR is signaled if no such file exists,
    or if PATHSPEC is a wild pathname.
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `FILL`

```lisp
COMMON-LISP:FILL
  [symbol]

FILL names a compiled function:
  Lambda-list: (SEQUENCE ITEM &KEY (START 0) END)
  Declared type: (FUNCTION
                  (SEQUENCE T &REST T &KEY (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:START . #1=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #1#)))
                 (VALUES SEQUENCE &OPTIONAL))
  Documentation:
    Replace the specified elements of SEQUENCE with ITEM.
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `FILL-POINTER`

```lisp
COMMON-LISP:FILL-POINTER
  [symbol]

FILL-POINTER names a compiled function:
  Lambda-list: (VECTOR)
  Declared type: (FUNCTION ((AND VECTOR (NOT SIMPLE-ARRAY)))
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Documentation:
    Return the FILL-POINTER of the given VECTOR.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

(SETF FILL-POINTER) names a compiled function:
  Lambda-list: (NEWVAL VECTOR)
  Derived type: (FUNCTION
                 ((UNSIGNED-BYTE 45) (AND VECTOR (NOT SIMPLE-ARRAY)))
                 (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF FILL-POINTER) has setf-expansion: SB-KERNEL:%SET-FILL-POINTER

```

### `(SETF FILL-POINTER)`

```lisp
COMMON-LISP-USER::|(SETF FILL-POINTER)|
  [symbol]

```

### `FIND`

```lisp
COMMON-LISP:FIND
  [symbol]

FIND names a compiled function:
  Lambda-list: (ITEM SEQUENCE &REST ARGS &KEY FROM-END (START 0) END
                KEY TEST TEST-NOT)
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T SEQUENCE &REST T &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))) (:FROM-END T)
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &REST T &KEY (:TEST . #1=((OR FUNCTION SYMBOL)))
                  (:TEST-NOT . #1#) (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:FROM-END T) (:KEY . #1#))
                 (VALUES T &OPTIONAL))
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `FIND-ALL-SYMBOLS`

```lisp
COMMON-LISP:FIND-ALL-SYMBOLS
  [symbol]

FIND-ALL-SYMBOLS names a compiled function:
  Lambda-list: (STRING-DESIGNATOR)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER))
                  (VALUES LIST &OPTIONAL))
  Documentation:
    Return a list of all symbols in the system having the specified name.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;DEFPACKAGE.LISP

```

### `FIND-CLASS`

```lisp
COMMON-LISP:FIND-CLASS
  [symbol]

FIND-CLASS names a compiled function:
  Lambda-list: (SYMBOL &OPTIONAL (ERRORP T) ENVIRONMENT)
  Declared type: (FUNCTION
                  (SYMBOL &OPTIONAL T (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES (OR CLASS NULL) &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T T) (VALUES T &OPTIONAL))
  Source file: SYS:SRC;PCL;MACROS.LISP

FIND-CLASS has a compiler-macro:
  Source file: SYS:SRC;PCL;MACROS.LISP

(SETF FIND-CLASS) names a compiled function:
  Lambda-list: (NEW-VALUE NAME &OPTIONAL ERRORP ENVIRONMENT)
  Derived type: (FUNCTION (T T &OPTIONAL T T) (VALUES T &OPTIONAL))
  Source file: SYS:SRC;PCL;MACROS.LISP

```

### `(SETF FIND-CLASS)`

```lisp
COMMON-LISP-USER::|(SETF FIND-CLASS)|
  [symbol]

```

### `FIND-IF`

```lisp
COMMON-LISP:FIND-IF
  [symbol]

FIND-IF names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                END KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `FIND-IF-NOT`

```lisp
COMMON-LISP:FIND-IF-NOT
  [symbol]

FIND-IF-NOT names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                END KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `FIND-PACKAGE`

```lisp
COMMON-LISP:FIND-PACKAGE
  [symbol]

FIND-PACKAGE names a compiled function:
  Lambda-list: (PACKAGE-DESIGNATOR)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (OR PACKAGE NULL) &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (OR PACKAGE NULL) &OPTIONAL))
  Documentation:
    If PACKAGE-DESIGNATOR is a package, it is returned. Otherwise PACKAGE-DESIGNATOR
    must be a string designator, in which case the package it names is located and returned.
    
    As an SBCL extension, the current package may affect the way a package name is
    resolved: if the current package has local nicknames specified, package names
    matching those are resolved to the packages associated with them instead.
    
    Example:
    
      (defpackage :a)
      (defpackage :example (:use :cl) (:local-nicknames (:x :a)))
      (let ((*package* (find-package :example)))
        (find-package :x)) => #<PACKAGE A>
    
    See also: ADD-PACKAGE-LOCAL-NICKNAME, PACKAGE-LOCAL-NICKNAMES,
    REMOVE-PACKAGE-LOCAL-NICKNAME, and the DEFPACKAGE option :LOCAL-NICKNAMES.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `FIND-RESTART`

```lisp
COMMON-LISP:FIND-RESTART
  [symbol]

FIND-RESTART names a compiled function:
  Lambda-list: (IDENTIFIER &OPTIONAL CONDITION)
  Declared type: (FUNCTION
                  ((OR (AND SYMBOL (NOT NULL)) RESTART) &OPTIONAL
                   (OR CONDITION NULL))
                  (VALUES (OR RESTART NULL) &OPTIONAL))
  Documentation:
    Return the first restart identified by IDENTIFIER. If IDENTIFIER is a symbol,
    then the innermost applicable restart with that name is returned. If IDENTIFIER
    is a restart, it is returned if it is currently active. Otherwise NIL is
    returned. If CONDITION is specified and not NIL, then only restarts associated
    with that condition (or with no condition) will be returned.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `FIND-SYMBOL`

```lisp
COMMON-LISP:FIND-SYMBOL
  [symbol]

FIND-SYMBOL names a compiled function:
  Lambda-list: (NAME &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  (STRING &OPTIONAL
                          (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES SYMBOL
                          (MEMBER NIL :INHERITED :EXTERNAL :INTERNAL)
                          &OPTIONAL))
  Documentation:
    Return the symbol named STRING in PACKAGE. If such a symbol is found
      then the second value is :INTERNAL, :EXTERNAL or :INHERITED to indicate
      how the symbol is accessible. If no symbol is found then both values
      are NIL.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `FINISH-OUTPUT`

```lisp
COMMON-LISP:FINISH-OUTPUT
  [symbol]

FINISH-OUTPUT names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T) (VALUES NULL &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `FIRST`

```lisp
COMMON-LISP:FIRST
  [symbol]

FIRST names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 1st object in a list or NIL if the list is empty.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF FIRST) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF FIRST) has setf-expansion: SB-KERNEL:%RPLACA

```

### `(SETF FIRST)`

```lisp
COMMON-LISP-USER::|(SETF FIRST)|
  [symbol]

```

### `FLOAT`

```lisp
COMMON-LISP:FLOAT
  [symbol]

FLOAT names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (OTHER NIL OTHERP))
  Declared type: (FUNCTION (REAL &OPTIONAL FLOAT)
                  (VALUES FLOAT &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES FLOAT &OPTIONAL))
  Documentation:
    Converts any REAL to a float. If OTHER is not provided, it returns a
      SINGLE-FLOAT if NUMBER is not already a FLOAT. If OTHER is provided, the
      result is the same float format as OTHER.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

FLOAT names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:FLOAT>:
  Class precedence-list: FLOAT, REAL, NUMBER, T
  Direct superclasses: REAL
  Direct subclasses: DOUBLE-FLOAT, SINGLE-FLOAT
  Sealed.
  No direct slots.

FLOAT names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (LOW (QUOTE *)) (HIGH (QUOTE *)))

```

### `FLOAT-DIGITS`

```lisp
COMMON-LISP:FLOAT-DIGITS
  [symbol]

FLOAT-DIGITS names a compiled function:
  Lambda-list: (F)
  Declared type: (FUNCTION (FLOAT) (VALUES (MOD 54) &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES (OR (INTEGER 24 24) (INTEGER 53 53))
                         &OPTIONAL))
  Documentation:
    Return a non-negative number of radix-b digits used in the
       representation of its argument.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

```

### `FLOAT-PRECISION`

```lisp
COMMON-LISP:FLOAT-PRECISION
  [symbol]

FLOAT-PRECISION names a compiled function:
  Lambda-list: (F)
  Declared type: (FUNCTION (FLOAT) (VALUES (MOD 54) &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (MOD 54) &OPTIONAL))
  Documentation:
    Return a non-negative number of significant digits in its float argument.
      Will be less than FLOAT-DIGITS if denormalized or zero.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

```

### `FLOAT-RADIX`

```lisp
COMMON-LISP:FLOAT-RADIX
  [symbol]

FLOAT-RADIX names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION (FLOAT) (VALUES (INTEGER 2 2) &OPTIONAL))
  Documentation:
    Return (as an integer) the radix b of its floating-point argument.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

```

### `FLOAT-SIGN`

```lisp
COMMON-LISP:FLOAT-SIGN
  [symbol]

FLOAT-SIGN names a compiled function:
  Lambda-list: (FLOAT1 &OPTIONAL (FLOAT2 (FLOAT 1 FLOAT1)))
  Declared type: (FUNCTION (FLOAT &OPTIONAL FLOAT)
                  (VALUES FLOAT &OPTIONAL))
  Documentation:
    Return a floating-point number that has the same sign as
       FLOAT1 and, if FLOAT2 is given, has the same absolute value
       as FLOAT2.
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

```

### `FLOATP`

```lisp
COMMON-LISP:FLOATP
  [symbol]

FLOATP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a FLOAT, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `FLOOR`

```lisp
COMMON-LISP:FLOOR
  [symbol]

FLOOR names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES INTEGER REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR NULL INTEGER) NUMBER &OPTIONAL))
  Documentation:
    Return the greatest integer not greater than number, or number/divisor.
      The second returned value is (mod number divisor).
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `FMAKUNBOUND`

```lisp
COMMON-LISP:FMAKUNBOUND
  [symbol]

FMAKUNBOUND names a compiled function:
  Lambda-list: (NAME)
  Declared type: (FUNCTION ((OR CONS SYMBOL))
                  (VALUES (OR CONS SYMBOL) &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (OR CONS SYMBOL) &OPTIONAL))
  Documentation:
    Make NAME have no global function definition.
  Source file: SYS:SRC;CODE;FDEFINITION.LISP

```

### `FORCE-OUTPUT`

```lisp
COMMON-LISP:FORCE-OUTPUT
  [symbol]

FORCE-OUTPUT names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T) (VALUES NULL &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `FORMAT`

```lisp
COMMON-LISP:FORMAT
  [symbol]

FORMAT names a compiled function:
  Lambda-list: (DESTINATION CONTROL-STRING &REST FORMAT-ARGUMENTS)
  Declared type: (FUNCTION
                  ((OR STRING BOOLEAN STREAM) (OR STRING FUNCTION)
                   &REST T)
                  (VALUES (OR STRING NULL) &OPTIONAL))
  Derived type: (FUNCTION (T T &REST T)
                 (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Provides various facilities for formatting output.
      CONTROL-STRING contains a string to be output, possibly with embedded
      directives, which are flagged with the escape character "~". Directives
      generally expand into additional text to be output, usually consuming one
      or more of the FORMAT-ARGUMENTS in the process. A few useful directives
      are:
            ~A or ~nA   Prints one argument as if by PRINC
            ~S or ~nS   Prints one argument as if by PRIN1
            ~D or ~nD   Prints one argument as a decimal integer
            ~%          Does a TERPRI
            ~&          Does a FRESH-LINE
      where n is the width of the field in which the object is printed.
    
      DESTINATION controls where the result will go. If DESTINATION is T, then
      the output is sent to the standard output stream. If it is NIL, then the
      output is returned in a string as the value of the call. Otherwise,
      DESTINATION must be a stream to which the output will be sent.
    
      Example:   (FORMAT NIL "The answer is ~D." 10) => "The answer is 10."
    
      FORMAT has many additional capabilities not described here. Consult the
      manual for details.
  Source file: SYS:SRC;CODE;TARGET-FORMAT.LISP

FORMAT has a compiler-macro:
  Source file: SYS:SRC;CODE;CMACROS.LISP

```

### `FOURTH`

```lisp
COMMON-LISP:FOURTH
  [symbol]

FOURTH names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 4th object in a list or NIL if there is no 4th object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF FOURTH) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF FOURTH) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF FOURTH)`

```lisp
COMMON-LISP-USER::|(SETF FOURTH)|
  [symbol]

```

### `FRESH-LINE`

```lisp
COMMON-LISP:FRESH-LINE
  [symbol]

FRESH-LINE names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T) *)
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `FROUND`

```lisp
COMMON-LISP:FROUND
  [symbol]

FROUND names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES FLOAT REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES FLOAT REAL &OPTIONAL))
  Documentation:
    Same as ROUND, but returns first value as a float.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `FTRUNCATE`

```lisp
COMMON-LISP:FTRUNCATE
  [symbol]

FTRUNCATE names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES FLOAT REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES FLOAT REAL &OPTIONAL))
  Documentation:
    Same as TRUNCATE, but returns first value as a float.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `FUNCALL`

```lisp
COMMON-LISP:FUNCALL
  [symbol]

FUNCALL names a compiled function:
  Lambda-list: (FUNCTION &REST ARGUMENTS)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) &REST T) *)
  Documentation:
    Call FUNCTION with the given ARGUMENTS.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;EVAL.LISP

```

### `FUNCTION-LAMBDA-EXPRESSION`

```lisp
COMMON-LISP:FUNCTION-LAMBDA-EXPRESSION
  [symbol]

FUNCTION-LAMBDA-EXPRESSION names a compiled function:
  Lambda-list: (FUN)
  Declared type: (FUNCTION (FUNCTION) (VALUES T BOOLEAN T &OPTIONAL))
  Documentation:
    Return (VALUES DEFINING-LAMBDA-EXPRESSION CLOSURE-P NAME), where
      DEFINING-LAMBDA-EXPRESSION is NIL if unknown, or a suitable argument
      to COMPILE otherwise, CLOSURE-P is non-NIL if the function's definition
      might have been enclosed in some non-null lexical environment, and
      NAME is some name (for debugging only) or NIL if there is no name.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;DESCRIBE.LISP

```

### `FUNCTIONP`

```lisp
COMMON-LISP:FUNCTIONP
  [symbol]

FUNCTIONP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a FUNCTION, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `GCD`

```lisp
COMMON-LISP:GCD
  [symbol]

GCD names a compiled function:
  Lambda-list: (&REST INTEGERS)
  Declared type: (FUNCTION (&REST INTEGER)
                  (VALUES UNSIGNED-BYTE &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the greatest common divisor of the arguments, which must be
      integers. GCD with no arguments is defined to be 0.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `GENSYM`

```lisp
COMMON-LISP:GENSYM
  [symbol]

GENSYM names a compiled function:
  Lambda-list: (&OPTIONAL (THING G))
  Declared type: (FUNCTION (&OPTIONAL (OR STRING UNSIGNED-BYTE))
                  (VALUES SYMBOL &OPTIONAL))
  Documentation:
    Creates a new uninterned symbol whose name is a prefix string (defaults
       to "G"), followed by a decimal number. Thing, when supplied, will
       alter the prefix if it is a string, or be used for the decimal number
       if it is a number, of this symbol. The default value of the number is
       the current value of *gensym-counter* which is incremented each time
       it is used.
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `GENTEMP`

```lisp
COMMON-LISP:GENTEMP
  [symbol]

GENTEMP names a compiled function:
  Lambda-list: (&OPTIONAL (PREFIX T) (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  (&OPTIONAL STRING
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES SYMBOL &OPTIONAL))
  Documentation:
    Creates a new symbol interned in package PACKAGE with the given PREFIX.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `GET`

```lisp
COMMON-LISP:GET
  [symbol]

GET names a compiled function:
  Lambda-list: (SYMBOL INDICATOR &OPTIONAL (DEFAULT NIL))
  Declared type: (FUNCTION (SYMBOL T &OPTIONAL T) (VALUES T &OPTIONAL))
  Documentation:
    Look on the property list of SYMBOL for the specified INDICATOR. If this
      is found, return the associated value, else return DEFAULT.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

(SETF GET) has a complex setf-expansion:
  Lambda-list: (SYMBOL INDICATOR &OPTIONAL DEFAULT)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `GET-DECODED-TIME`

```lisp
COMMON-LISP:GET-DECODED-TIME
  [symbol]

GET-DECODED-TIME names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL
                  (VALUES (MOD 60) (MOD 60) (MOD 24) (INTEGER 1 31)
                          (INTEGER 1 12) UNSIGNED-BYTE (MOD 7) BOOLEAN
                          (RATIONAL -24 24) &OPTIONAL))
  Documentation:
    Return nine values specifying the current time as follows:
       second, minute, hour, date, month, year, day of week (0 = Monday), T
       (daylight savings times) or NIL (standard time), and timezone.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TIME.LISP

```

### `GET-DISPATCH-MACRO-CHARACTER`

```lisp
COMMON-LISP:GET-DISPATCH-MACRO-CHARACTER
  [symbol]

GET-DISPATCH-MACRO-CHARACTER names a compiled function:
  Lambda-list: (DISP-CHAR SUB-CHAR &OPTIONAL
                (RT-DESIGNATOR *READTABLE*))
  Declared type: (FUNCTION
                  (CHARACTER CHARACTER &OPTIONAL (OR READTABLE NULL))
                  (VALUES (OR FUNCTION SYMBOL) &OPTIONAL))
  Documentation:
    Return the macro character function for SUB-CHAR under DISP-CHAR
       or NIL if there is no associated function.
  Source file: SYS:SRC;CODE;READER.LISP

```

### `GET-INTERNAL-REAL-TIME`

```lisp
COMMON-LISP:GET-INTERNAL-REAL-TIME
  [symbol]

GET-INTERNAL-REAL-TIME names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL (VALUES (UNSIGNED-BYTE 61) &OPTIONAL))
  Documentation:
    Return the real time ("wallclock time") since startup in the internal
    time format. (See INTERNAL-TIME-UNITS-PER-SECOND.)
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;WIN32.LISP

```

### `GET-INTERNAL-RUN-TIME`

```lisp
COMMON-LISP:GET-INTERNAL-RUN-TIME
  [symbol]

GET-INTERNAL-RUN-TIME names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL (VALUES (UNSIGNED-BYTE 61) &OPTIONAL))
  Documentation:
    Return the run time used by the process in the internal time format. (See
    INTERNAL-TIME-UNITS-PER-SECOND.) This is useful for finding CPU usage.
    Includes both "system" and "user" time.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TIME.LISP

```

### `GET-MACRO-CHARACTER`

```lisp
COMMON-LISP:GET-MACRO-CHARACTER
  [symbol]

GET-MACRO-CHARACTER names a compiled function:
  Lambda-list: (CHAR &OPTIONAL (RT-DESIGNATOR *READTABLE*))
  Declared type: (FUNCTION (CHARACTER &OPTIONAL (OR READTABLE NULL))
                  (VALUES (OR FUNCTION SYMBOL) BOOLEAN &OPTIONAL))
  Documentation:
    Return the function associated with the specified CHAR which is a macro
      character, or NIL if there is no such function. As a second value, return
      T if CHAR is a macro character which is non-terminating, i.e. which can
      be embedded in a symbol name.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;READER.LISP

```

### `GET-OUTPUT-STREAM-STRING`

```lisp
COMMON-LISP:GET-OUTPUT-STREAM-STRING
  [symbol]

GET-OUTPUT-STREAM-STRING names a compiled function:
  Lambda-list: (STREAM)
  Declared type: (FUNCTION (STREAM) (VALUES SIMPLE-STRING &OPTIONAL))
  Derived type: (FUNCTION (SB-IMPL::STRING-OUTPUT-STREAM)
                 (VALUES SIMPLE-STRING &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `GET-PROPERTIES`

```lisp
COMMON-LISP:GET-PROPERTIES
  [symbol]

GET-PROPERTIES names a compiled function:
  Lambda-list: (PLACE INDICATOR-LIST)
  Declared type: (FUNCTION (LIST LIST) (VALUES T T LIST &OPTIONAL))
  Documentation:
    Like GETF, except that INDICATOR-LIST is a list of indicators which will
      be looked for in the property list stored in PLACE. Three values are
      returned, see manual for details.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `GET-SETF-EXPANSION`

```lisp
COMMON-LISP:GET-SETF-EXPANSION
  [symbol]

GET-SETF-EXPANSION names a compiled function:
  Lambda-list: (FORM &OPTIONAL ENVIRONMENT)
  Declared type: (FUNCTION
                  (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL)) *)
  Documentation:
    Return five values needed by the SETF machinery: a list of temporary
       variables, a list of values with which to fill them, a list of temporaries
       for the new values, the setting function, and the accessing function.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `GET-UNIVERSAL-TIME`

```lisp
COMMON-LISP:GET-UNIVERSAL-TIME
  [symbol]

GET-UNIVERSAL-TIME names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL (VALUES UNSIGNED-BYTE &OPTIONAL))
  Derived type: (FUNCTION NIL (VALUES (MOD 1835238922571) &OPTIONAL))
  Documentation:
    Return a single integer for the current time of day in universal time
    format.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TIME.LISP

```

### `GETF`

```lisp
COMMON-LISP:GETF
  [symbol]

GETF names a compiled function:
  Lambda-list: (PLACE INDICATOR &OPTIONAL (DEFAULT NIL))
  Declared type: (FUNCTION (LIST T &OPTIONAL T) (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T T &OPTIONAL T) (VALUES T &OPTIONAL))
  Documentation:
    Search the property list stored in PLACE for an indicator EQ to INDICATOR.
      If one is found, return the corresponding value, else return DEFAULT.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

(SETF GETF) has a complex setf-expansion:
  Lambda-list: (PLACE PROP &OPTIONAL DEFAULT)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `GETHASH`

```lisp
COMMON-LISP:GETHASH
  [symbol]

GETHASH names a compiled function:
  Lambda-list: (KEY HASH-TABLE &OPTIONAL DEFAULT)
  Declared type: (FUNCTION (T HASH-TABLE &OPTIONAL T)
                  (VALUES T BOOLEAN &OPTIONAL))
  Documentation:
    Finds the entry in HASH-TABLE whose key is KEY and returns the
    associated value and T as multiple values, or returns DEFAULT and NIL
    if there is no such entry. Entries can be added using SETF.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-HASH-TABLE.LISP

(SETF GETHASH) names a compiled function:
  Lambda-list: (NEW-VALUE KEY TABLE &OPTIONAL DEFAULT)
  Derived type: (FUNCTION (T T T &OPTIONAL T) (VALUES T &OPTIONAL))
  Source file: SYS:SRC;CODE;TARGET-HASH-TABLE.LISP

(SETF GETHASH) has a complex setf-expansion:
  Lambda-list: (KEY HASHTABLE &OPTIONAL DEFAULT)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF GETHASH)`

```lisp
COMMON-LISP-USER::|(SETF GETHASH)|
  [symbol]

```

### `GRAPHIC-CHAR-P`

```lisp
COMMON-LISP:GRAPHIC-CHAR-P
  [symbol]

GRAPHIC-CHAR-P names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    The argument must be a character object. GRAPHIC-CHAR-P returns T if the
    argument is a printing character (space through ~ in ASCII), otherwise returns
    NIL.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `HASH-TABLE-COUNT`

```lisp
COMMON-LISP:HASH-TABLE-COUNT
  [symbol]

HASH-TABLE-COUNT names a compiled function:
  Lambda-list: (HASH-TABLE)
  Declared type: (FUNCTION (HASH-TABLE)
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Documentation:
    Return the number of entries in the given HASH-TABLE.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-HASH-TABLE.LISP

```

### `HASH-TABLE-P`

```lisp
COMMON-LISP:HASH-TABLE-P
  [symbol]

HASH-TABLE-P names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;HASH-TABLE.LISP

```

### `HASH-TABLE-REHASH-SIZE`

```lisp
COMMON-LISP:HASH-TABLE-REHASH-SIZE
  [symbol]

HASH-TABLE-REHASH-SIZE names a compiled function:
  Lambda-list: (INSTANCE)
  Declared type: (FUNCTION (HASH-TABLE)
                  (VALUES (OR (UNSIGNED-BYTE 45) (SINGLE-FLOAT (1.0)))
                          &OPTIONAL))
  Documentation:
    Return the rehash-size HASH-TABLE was created with.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;HASH-TABLE.LISP

```

### `HASH-TABLE-REHASH-THRESHOLD`

```lisp
COMMON-LISP:HASH-TABLE-REHASH-THRESHOLD
  [symbol]

HASH-TABLE-REHASH-THRESHOLD names a compiled function:
  Lambda-list: (INSTANCE)
  Declared type: (FUNCTION (HASH-TABLE)
                  (VALUES (SINGLE-FLOAT (0.0) 1.0) &OPTIONAL))
  Documentation:
    Return the rehash-threshold HASH-TABLE was created with.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;HASH-TABLE.LISP

```

### `HASH-TABLE-SIZE`

```lisp
COMMON-LISP:HASH-TABLE-SIZE
  [symbol]

HASH-TABLE-SIZE names a compiled function:
  Lambda-list: (HASH-TABLE)
  Declared type: (FUNCTION (HASH-TABLE)
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION (HASH-TABLE)
                 (VALUES (INTEGER 1 17592186044414) &OPTIONAL))
  Documentation:
    Return a size that can be used with MAKE-HASH-TABLE to create a hash
       table that can hold however many entries HASH-TABLE can hold without
       having to be grown.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-HASH-TABLE.LISP

```

### `HASH-TABLE-TEST`

```lisp
COMMON-LISP:HASH-TABLE-TEST
  [symbol]

HASH-TABLE-TEST names a compiled function:
  Lambda-list: (INSTANCE)
  Declared type: (FUNCTION (HASH-TABLE)
                  (VALUES (OR FUNCTION SYMBOL) &OPTIONAL))
  Documentation:
    Return the test HASH-TABLE was created with.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;HASH-TABLE.LISP

```

### `HOST-NAMESTRING`

```lisp
COMMON-LISP:HOST-NAMESTRING
  [symbol]

HOST-NAMESTRING names a compiled function:
  Lambda-list: (PATHNAME)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Return a string representation of the name of the host in PATHNAME.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `IDENTITY`

```lisp
COMMON-LISP:IDENTITY
  [symbol]

IDENTITY names a compiled function:
  Lambda-list: (THING)
  Declared type: (FUNCTION (T) (VALUES T &OPTIONAL))
  Documentation:
    This function simply returns what was passed to it.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FUNUTILS.LISP

```

### `IMAGPART`

```lisp
COMMON-LISP:IMAGPART
  [symbol]

IMAGPART names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES REAL &OPTIONAL))
  Documentation:
    Extract the imaginary part of a number.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `IMPORT`

```lisp
COMMON-LISP:IMPORT
  [symbol]

IMPORT names a compiled function:
  Lambda-list: (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Make SYMBOLS accessible as internal symbols in PACKAGE. If a symbol is
    already accessible then it has no effect. If a name conflict would result from
    the importation, then a correctable error is signalled.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `INSPECT`

```lisp
COMMON-LISP:INSPECT
  [symbol]

INSPECT names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES &OPTIONAL))
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;INSPECT.LISP

```

### `INTEGER-DECODE-FLOAT`

```lisp
COMMON-LISP:INTEGER-DECODE-FLOAT
  [symbol]

INTEGER-DECODE-FLOAT names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION (FLOAT)
                  (VALUES (UNSIGNED-BYTE 53) (INTEGER -1127 971)
                          (OR (INTEGER 1 1) (INTEGER -1 -1)) &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES (UNSIGNED-BYTE 53) (INTEGER -1127 971)
                         (OR (INTEGER 1 1) (INTEGER -1 -1)) &OPTIONAL))
  Documentation:
    Return three values:
       1) an integer representation of the significand.
       2) the exponent for the power of 2 that the significand must be multiplied
          by to get the actual value. This differs from the DECODE-FLOAT exponent
          by FLOAT-DIGITS, since the significand has been scaled to have all its
          digits before the radix point.
       3) -1 or 1 (i.e. the sign of the argument.)
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

```

### `INTEGER-LENGTH`

```lisp
COMMON-LISP:INTEGER-LENGTH
  [symbol]

INTEGER-LENGTH names a compiled function:
  Lambda-list: (INTEGER)
  Declared type: (FUNCTION (INTEGER)
                  (VALUES (UNSIGNED-BYTE 38) &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (MOD 274877906880) &OPTIONAL))
  Documentation:
    Return the number of non-sign bits in the twos-complement representation
      of INTEGER.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `INTEGERP`

```lisp
COMMON-LISP:INTEGERP
  [symbol]

INTEGERP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is an INTEGER, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `INTERN`

```lisp
COMMON-LISP:INTERN
  [symbol]

INTERN names a compiled function:
  Lambda-list: (NAME &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  (STRING &OPTIONAL
                          (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES SYMBOL
                          (MEMBER NIL :INHERITED :EXTERNAL :INTERNAL)
                          &OPTIONAL))
  Documentation:
    Return a symbol in PACKAGE having the specified NAME, creating it
      if necessary.
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `INTERSECTION`

```lisp
COMMON-LISP:INTERSECTION
  [symbol]

INTERSECTION names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Return the intersection of LIST1 and LIST2.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `INVALID-METHOD-ERROR`

```lisp
COMMON-LISP:INVALID-METHOD-ERROR
  [symbol]

INVALID-METHOD-ERROR names a compiled function:
  Lambda-list: (METHOD FORMAT-CONTROL &REST FORMAT-ARGUMENTS)
  Declared type: (FUNCTION (T (OR STRING FUNCTION) &REST T) *)
  Derived type: (FUNCTION (T (OR STRING FUNCTION) &REST T) NIL)
  Known attributes: unwind, any
  Source file: SYS:SRC;PCL;COMBIN.LISP

```

### `INVOKE-DEBUGGER`

```lisp
COMMON-LISP:INVOKE-DEBUGGER
  [symbol]

INVOKE-DEBUGGER names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (CONDITION) NIL)
  Documentation:
    Enter the debugger.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;DEBUG.LISP

```

### `INVOKE-RESTART`

```lisp
COMMON-LISP:INVOKE-RESTART
  [symbol]

INVOKE-RESTART names a compiled function:
  Lambda-list: (RESTART &REST VALUES)
  Declared type: (FUNCTION
                  ((OR (AND SYMBOL (NOT NULL)) RESTART) &REST T) *)
  Documentation:
    Calls the function associated with the given restart, passing any given
       arguments. If the argument restart is not a restart or a currently active
       non-nil restart name, then a CONTROL-ERROR is signalled.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `INVOKE-RESTART-INTERACTIVELY`

```lisp
COMMON-LISP:INVOKE-RESTART-INTERACTIVELY
  [symbol]

INVOKE-RESTART-INTERACTIVELY names a compiled function:
  Lambda-list: (RESTART)
  Declared type: (FUNCTION ((OR (AND SYMBOL (NOT NULL)) RESTART)) *)
  Documentation:
    Calls the function associated with the given restart, prompting for any
       necessary arguments. If the argument restart is not a restart or a
       currently active non-NIL restart name, then a CONTROL-ERROR is signalled.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `ISQRT`

```lisp
COMMON-LISP:ISQRT
  [symbol]

ISQRT names a compiled function:
  Lambda-list: (N)
  Declared type: (FUNCTION (UNSIGNED-BYTE)
                  (VALUES UNSIGNED-BYTE &OPTIONAL))
  Derived type: (FUNCTION (UNSIGNED-BYTE)
                 (VALUES (INTEGER -1) &OPTIONAL))
  Documentation:
    Return the greatest integer less than or equal to the square root of N.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `KEYWORDP`

```lisp
COMMON-LISP:KEYWORDP
  [symbol]

KEYWORDP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if Object is a symbol in the "KEYWORD" package.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: flushable, unsafely-flushable, predicate
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `LAST`

```lisp
COMMON-LISP:LAST
  [symbol]

LAST names a compiled function:
  Lambda-list: (LIST &OPTIONAL (N 1))
  Declared type: (FUNCTION (LIST &OPTIONAL UNSIGNED-BYTE)
                  (VALUES T &OPTIONAL))
  Documentation:
    Return the last N conses (not the last element!) of a list.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

LAST has a compiler-macro:
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `LCM`

```lisp
COMMON-LISP:LCM
  [symbol]

LCM names a compiled function:
  Lambda-list: (&REST INTEGERS)
  Declared type: (FUNCTION (&REST INTEGER)
                  (VALUES UNSIGNED-BYTE &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the least common multiple of one or more integers. LCM of no
      arguments is defined to be 1.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LDB`

```lisp
COMMON-LISP:LDB
  [symbol]

LDB names a compiled function:
  Lambda-list: (BYTESPEC INTEGER)
  Declared type: (FUNCTION (CONS INTEGER)
                  (VALUES UNSIGNED-BYTE &OPTIONAL))
  Documentation:
    Extract the specified byte from integer, and right justify result.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

(SETF LDB) has a complex setf-expansion:
  Lambda-list: (SPEC PLACE)
  Documentation:
    The first argument is a byte specifier. The second is any place form
    acceptable to SETF. Replace the specified byte of the number in this
    place with bits from the low-order end of the new value.
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `LDB-TEST`

```lisp
COMMON-LISP:LDB-TEST
  [symbol]

LDB-TEST names a compiled function:
  Lambda-list: (BYTESPEC INTEGER)
  Declared type: (FUNCTION (CONS INTEGER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if any of the specified bits in integer are 1's.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LDIFF`

```lisp
COMMON-LISP:LDIFF
  [symbol]

LDIFF names a compiled function:
  Lambda-list: (LIST OBJECT)
  Declared type: (FUNCTION (LIST T) (VALUES LIST &OPTIONAL))
  Documentation:
    Return a new list, whose elements are those of LIST that appear before
       OBJECT. If OBJECT is not a tail of LIST, a copy of LIST is returned.
       LIST must be a proper list or a dotted list.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `LENGTH`

```lisp
COMMON-LISP:LENGTH
  [symbol]

LENGTH names a compiled function:
  Lambda-list: (SEQUENCE)
  Declared type: (FUNCTION (SEQUENCE)
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return an integer that is the length of SEQUENCE.
  Known attributes: dx-safe, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `LISP-IMPLEMENTATION-TYPE`

```lisp
COMMON-LISP:LISP-IMPLEMENTATION-TYPE
  [symbol]

LISP-IMPLEMENTATION-TYPE names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL (VALUES SIMPLE-STRING &OPTIONAL))
  Derived type: (FUNCTION NIL (VALUES (SIMPLE-BASE-STRING 4) &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;MISC.LISP

```

### `LISP-IMPLEMENTATION-VERSION`

```lisp
COMMON-LISP:LISP-IMPLEMENTATION-VERSION
  [symbol]

LISP-IMPLEMENTATION-VERSION names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL (VALUES SIMPLE-STRING &OPTIONAL))
  Derived type: (FUNCTION NIL (VALUES (SIMPLE-BASE-STRING 5) &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;MISC.LISP

```

### `LIST`

```lisp
COMMON-LISP:LIST
  [symbol]

LIST names a compiled function:
  Lambda-list: (&REST ARGS)
  Declared type: (FUNCTION * (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES LIST &OPTIONAL))
  Documentation:
    Construct and return a list containing the objects ARGS.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;LIST.LISP

LIST names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:LIST>:
  Class precedence-list: LIST, SEQUENCE, T
  Direct superclasses: SEQUENCE
  Direct subclasses: CONS, NULL
  Sealed.
  No direct slots.

```

### `LIST*`

```lisp
COMMON-LISP:LIST*
  [symbol]

LIST* names a compiled function:
  Lambda-list: (ARG &REST OTHERS)
  Declared type: (FUNCTION (T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Return a list of the arguments with last cons a dotted pair.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `LIST-ALL-PACKAGES`

```lisp
COMMON-LISP:LIST-ALL-PACKAGES
  [symbol]

LIST-ALL-PACKAGES names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL (VALUES LIST &OPTIONAL))
  Documentation:
    Return a list of all existing packages.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `LIST-LENGTH`

```lisp
COMMON-LISP:LIST-LENGTH
  [symbol]

LIST-LENGTH names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST)
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Return the length of LIST, or NIL if LIST is circular.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `LISTEN`

```lisp
COMMON-LISP:LISTEN
  [symbol]

LISTEN names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-INPUT*))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T) *)
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `LISTP`

```lisp
COMMON-LISP:LISTP
  [symbol]

LISTP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a LIST, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `LOAD`

```lisp
COMMON-LISP:LOAD
  [symbol]

LOAD names a compiled function:
  Lambda-list: (FILESPEC &KEY (VERBOSE *LOAD-VERBOSE*)
                (PRINT *LOAD-PRINT*) (IF-DOES-NOT-EXIST ERROR)
                (EXTERNAL-FORMAT DEFAULT))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME STREAM) &KEY (:VERBOSE T)
                   (:PRINT T) (:IF-DOES-NOT-EXIST T)
                   (:EXTERNAL-FORMAT (OR KEYWORD (CONS KEYWORD T))))
                  (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Load the file given by FILESPEC into the Lisp environment, returning T on
       success. The file type (a.k.a extension) is defaulted if missing. These
       options are defined:
    
       :IF-DOES-NOT-EXIST
           If :ERROR (the default), signal an error if the file can't be located.
           If NIL, simply return NIL (LOAD normally returns T.)
    
       :VERBOSE
           If true, print a line describing each file loaded.
    
       :PRINT
           If true, print information about loaded values.  When loading the
           source, the result of evaluating each top-level form is printed.
    
       :EXTERNAL-FORMAT
           The external-format to use when opening the FILENAME. The default is
           :DEFAULT which uses the SB-EXT:*DEFAULT-EXTERNAL-FORMAT*.
  Inline proclamation: NOTINLINE (no inline expansion available)
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-LOAD.LISP

```

### `LOAD-LOGICAL-PATHNAME-TRANSLATIONS`

```lisp
COMMON-LISP:LOAD-LOGICAL-PATHNAME-TRANSLATIONS
  [symbol]

LOAD-LOGICAL-PATHNAME-TRANSLATIONS names a compiled function:
  Lambda-list: (HOST)
  Declared type: (FUNCTION (STRING) (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (STRING) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Reads logical pathname translations from SYS:SITE;HOST.TRANSLATIONS.NEWEST,
    with HOST replaced by the supplied parameter. Returns T on success.
    
    If HOST is already defined as logical pathname host, no file is loaded and NIL
    is returned.
    
    The file should contain a single form, suitable for use with
    (SETF LOGICAL-PATHNAME-TRANSLATIONS).
    
    Note: behaviour of this function is highly implementation dependent, and
    historically it used to be a no-op in SBCL -- the current approach is somewhat
    experimental and subject to change.
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `LOG`

```lisp
COMMON-LISP:LOG
  [symbol]

LOG names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (BASE NIL BASE-P))
  Declared type: (FUNCTION (NUMBER &OPTIONAL REAL)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) *)
  Documentation:
    Return the logarithm of NUMBER in the base BASE, which defaults to e.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `LOGAND`

```lisp
COMMON-LISP:LOGAND
  [symbol]

LOGAND names a compiled function:
  Lambda-list: (&REST INTEGERS)
  Declared type: (FUNCTION (&REST INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the bit-wise and of its arguments. Args must be integers.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGANDC1`

```lisp
COMMON-LISP:LOGANDC1
  [symbol]

LOGANDC1 names a compiled function:
  Lambda-list: (INTEGER1 INTEGER2)
  Declared type: (FUNCTION (INTEGER INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES (OR NULL INTEGER) &OPTIONAL))
  Documentation:
    Bitwise AND (LOGNOT INTEGER1) with INTEGER2.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGANDC2`

```lisp
COMMON-LISP:LOGANDC2
  [symbol]

LOGANDC2 names a compiled function:
  Lambda-list: (INTEGER1 INTEGER2)
  Declared type: (FUNCTION (INTEGER INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES (OR NULL INTEGER) &OPTIONAL))
  Documentation:
    Bitwise AND INTEGER1 with (LOGNOT INTEGER2).
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGBITP`

```lisp
COMMON-LISP:LOGBITP
  [symbol]

LOGBITP names a compiled function:
  Lambda-list: (INDEX INTEGER)
  Declared type: (FUNCTION (UNSIGNED-BYTE INTEGER)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Predicate returns T if bit index of integer is a 1.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;NUMBERS.LISP

(SETF LOGBITP) has a complex setf-expansion:
  Lambda-list: (SPEC PLACE)
  Documentation:
    The first argument is a byte specifier. The second is any place form
    acceptable to SETF. Replace the specified byte of the number in this
    place with bits from the low-order end of the new value.
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `LOGCOUNT`

```lisp
COMMON-LISP:LOGCOUNT
  [symbol]

LOGCOUNT names a compiled function:
  Lambda-list: (INTEGER)
  Declared type: (FUNCTION (INTEGER)
                  (VALUES (UNSIGNED-BYTE 38) &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES (INTEGER -274877906816 274877906880)
                         &OPTIONAL))
  Documentation:
    Count the number of 1 bits if INTEGER is non-negative,
    and the number of 0 bits if INTEGER is negative.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGEQV`

```lisp
COMMON-LISP:LOGEQV
  [symbol]

LOGEQV names a compiled function:
  Lambda-list: (&REST INTEGERS)
  Declared type: (FUNCTION (&REST INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the bit-wise equivalence of its arguments. Args must be integers.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGICAL-PATHNAME`

```lisp
COMMON-LISP:LOGICAL-PATHNAME
  [symbol]

LOGICAL-PATHNAME names a compiled function:
  Lambda-list: (PATHSPEC)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES LOGICAL-PATHNAME &OPTIONAL))
  Derived type: (FUNCTION
                 ((OR STRING LOGICAL-PATHNAME SYNONYM-STREAM
                      FILE-STREAM))
                 (VALUES LOGICAL-PATHNAME &OPTIONAL))
  Documentation:
    Converts the pathspec argument to a logical-pathname and returns it.
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

LOGICAL-PATHNAME names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:LOGICAL-PATHNAME>:
  Class precedence-list: LOGICAL-PATHNAME, PATHNAME, T
  Direct superclasses: PATHNAME
  No subclasses.
  Sealed.
  No direct slots.

```

### `LOGICAL-PATHNAME-TRANSLATIONS`

```lisp
COMMON-LISP:LOGICAL-PATHNAME-TRANSLATIONS
  [symbol]

LOGICAL-PATHNAME-TRANSLATIONS names a compiled function:
  Lambda-list: (HOST)
  Declared type: (FUNCTION ((OR STRING SB-KERNEL:HOST))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION ((OR STRING SB-KERNEL:LOGICAL-HOST))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Return the (logical) host object argument's list of translations.
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

(SETF LOGICAL-PATHNAME-TRANSLATIONS) names a compiled function:
  Lambda-list: (TRANSLATIONS HOST)
  Derived type: (FUNCTION (LIST (OR STRING SB-KERNEL:LOGICAL-HOST))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Set the translations list for the logical host argument.
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `(SETF LOGICAL-PATHNAME-TRANSLATIONS)`

```lisp
COMMON-LISP-USER::|(SETF LOGICAL-PATHNAME-TRANSLATIONS)|
  [symbol]

```

### `LOGIOR`

```lisp
COMMON-LISP:LOGIOR
  [symbol]

LOGIOR names a compiled function:
  Lambda-list: (&REST INTEGERS)
  Declared type: (FUNCTION (&REST INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the bit-wise or of its arguments. Args must be integers.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGNAND`

```lisp
COMMON-LISP:LOGNAND
  [symbol]

LOGNAND names a compiled function:
  Lambda-list: (INTEGER1 INTEGER2)
  Declared type: (FUNCTION (INTEGER INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Complement the logical AND of INTEGER1 and INTEGER2.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGNOR`

```lisp
COMMON-LISP:LOGNOR
  [symbol]

LOGNOR names a compiled function:
  Lambda-list: (INTEGER1 INTEGER2)
  Declared type: (FUNCTION (INTEGER INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Complement the logical OR of INTEGER1 and INTEGER2.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGNOT`

```lisp
COMMON-LISP:LOGNOT
  [symbol]

LOGNOT names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the bit-wise logical not of integer.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGORC1`

```lisp
COMMON-LISP:LOGORC1
  [symbol]

LOGORC1 names a compiled function:
  Lambda-list: (INTEGER1 INTEGER2)
  Declared type: (FUNCTION (INTEGER INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES (OR NULL INTEGER) &OPTIONAL))
  Documentation:
    Bitwise OR (LOGNOT INTEGER1) with INTEGER2.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGORC2`

```lisp
COMMON-LISP:LOGORC2
  [symbol]

LOGORC2 names a compiled function:
  Lambda-list: (INTEGER1 INTEGER2)
  Declared type: (FUNCTION (INTEGER INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES (OR NULL INTEGER) &OPTIONAL))
  Documentation:
    Bitwise OR INTEGER1 with (LOGNOT INTEGER2).
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGTEST`

```lisp
COMMON-LISP:LOGTEST
  [symbol]

LOGTEST names a compiled function:
  Lambda-list: (INTEGER1 INTEGER2)
  Declared type: (FUNCTION (INTEGER INTEGER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Predicate which returns T if logand of integer1 and integer2 is not zero.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LOGXOR`

```lisp
COMMON-LISP:LOGXOR
  [symbol]

LOGXOR names a compiled function:
  Lambda-list: (&REST INTEGERS)
  Declared type: (FUNCTION (&REST INTEGER) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the bit-wise exclusive or of its arguments. Args must be integers.
  Known attributes: foldable, flushable, unsafely-flushable, movable, commutative
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `LONG-SITE-NAME`

```lisp
COMMON-LISP:LONG-SITE-NAME
  [symbol]

LONG-SITE-NAME names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Return a string with the long form of the site name, or NIL if not known.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-MISC.LISP

```

### `LOWER-CASE-P`

```lisp
COMMON-LISP:LOWER-CASE-P
  [symbol]

LOWER-CASE-P names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    The argument must be a character object; LOWER-CASE-P returns T if the
    argument is a lower-case character, NIL otherwise.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `MACHINE-INSTANCE`

```lisp
COMMON-LISP:MACHINE-INSTANCE
  [symbol]

MACHINE-INSTANCE names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Derived type: (FUNCTION NIL (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Return a string giving the name of the local machine.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-MISC.LISP

```

### `MACHINE-TYPE`

```lisp
COMMON-LISP:MACHINE-TYPE
  [symbol]

MACHINE-TYPE names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Derived type: (FUNCTION NIL (VALUES (SIMPLE-BASE-STRING 6) &OPTIONAL))
  Documentation:
    Return a string describing the type of the local machine.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;X86-64-VM.LISP

```

### `MACHINE-VERSION`

```lisp
COMMON-LISP:MACHINE-VERSION
  [symbol]

MACHINE-VERSION names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Return a string describing the version of the computer hardware we
    are running on, or NIL if we can't find any useful information.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-MISC.LISP

```

### `MACRO-FUNCTION`

```lisp
COMMON-LISP:MACRO-FUNCTION
  [symbol]

MACRO-FUNCTION names a compiled function:
  Lambda-list: (SYMBOL &OPTIONAL ENV)
  Declared type: (FUNCTION
                  (SYMBOL &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES (OR NULL FUNCTION) &OPTIONAL))
  Documentation:
    If SYMBOL names a macro in ENV, returns the expansion function,
    else returns NIL. If ENV is unspecified or NIL, use the global environment
    only.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;DEFMACRO.LISP

(SETF MACRO-FUNCTION) names a compiled function:
  Lambda-list: (FUNCTION SYMBOL &OPTIONAL ENVIRONMENT)
  Derived type: (FUNCTION (FUNCTION SYMBOL &OPTIONAL T)
                 (VALUES FUNCTION &OPTIONAL))
  Source file: SYS:SRC;CODE;DEFMACRO.LISP

```

### `(SETF MACRO-FUNCTION)`

```lisp
COMMON-LISP-USER::|(SETF MACRO-FUNCTION)|
  [symbol]

```

### `MACROEXPAND`

```lisp
COMMON-LISP:MACROEXPAND
  [symbol]

MACROEXPAND names a compiled function:
  Lambda-list: (FORM &OPTIONAL ENV)
  Declared type: (FUNCTION
                  (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES T &OPTIONAL BOOLEAN))
  Derived type: (FUNCTION (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                 (VALUES T BOOLEAN &OPTIONAL))
  Documentation:
    Repetitively call MACROEXPAND-1 until the form can no longer be expanded.
       Returns the final resultant form, and T if it was expanded. ENV is the
       lexical environment to expand in, or NIL (the default) for the null
       environment.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;MACROEXPAND.LISP

```

### `MACROEXPAND-1`

```lisp
COMMON-LISP:MACROEXPAND-1
  [symbol]

MACROEXPAND-1 names a compiled function:
  Lambda-list: (FORM &OPTIONAL ENV)
  Declared type: (FUNCTION
                  (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES T &OPTIONAL BOOLEAN))
  Derived type: (FUNCTION (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                 (VALUES T BOOLEAN &OPTIONAL))
  Documentation:
    If form is a macro (or symbol macro), expand it once. Return two values,
       the expanded form and a T-or-NIL flag indicating whether the form was, in
       fact, a macro. ENV is the lexical environment to expand in, which defaults
       to the null environment.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;MACROEXPAND.LISP

```

### `MAKE-ARRAY`

```lisp
COMMON-LISP:MAKE-ARRAY
  [symbol]

MAKE-ARRAY names a compiled function:
  Lambda-list: (DIMENSIONS &REST ARGS &KEY (ELEMENT-TYPE T)
                INITIAL-ELEMENT INITIAL-CONTENTS ADJUSTABLE
                FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET)
  Declared type: (FUNCTION
                  ((OR LIST (UNSIGNED-BYTE 45)) &KEY
                   (:ELEMENT-TYPE
                    (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS))
                   (:INITIAL-ELEMENT T) (:INITIAL-CONTENTS T)
                   (:ADJUSTABLE T)
                   (:FILL-POINTER (OR (UNSIGNED-BYTE 45) BOOLEAN))
                   (:DISPLACED-TO (OR ARRAY NULL))
                   (:DISPLACED-INDEX-OFFSET (UNSIGNED-BYTE 45)))
                  (VALUES ARRAY &OPTIONAL))
  Derived type: (FUNCTION
                 (T &REST T &KEY (:ELEMENT-TYPE . #1=(T))
                  (:INITIAL-ELEMENT . #1#) (:INITIAL-CONTENTS . #1#)
                  (:ADJUSTABLE . #1#) (:FILL-POINTER . #1#)
                  (:DISPLACED-TO . #1#)
                  (:DISPLACED-INDEX-OFFSET . #1#))
                 (VALUES ARRAY &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `MAKE-BROADCAST-STREAM`

```lisp
COMMON-LISP:MAKE-BROADCAST-STREAM
  [symbol]

MAKE-BROADCAST-STREAM names a compiled function:
  Lambda-list: (&REST STREAMS)
  Declared type: (FUNCTION (&REST STREAM)
                  (VALUES BROADCAST-STREAM &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES BROADCAST-STREAM &OPTIONAL))
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `MAKE-CONCATENATED-STREAM`

```lisp
COMMON-LISP:MAKE-CONCATENATED-STREAM
  [symbol]

MAKE-CONCATENATED-STREAM names a compiled function:
  Lambda-list: (&REST STREAMS)
  Declared type: (FUNCTION (&REST STREAM)
                  (VALUES CONCATENATED-STREAM &OPTIONAL))
  Derived type: (FUNCTION (&REST T)
                 (VALUES CONCATENATED-STREAM &OPTIONAL))
  Documentation:
    Return a stream which takes its input from each of the streams in turn,
       going on to the next at EOF.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `MAKE-CONDITION`

```lisp
COMMON-LISP:MAKE-CONDITION
  [symbol]

MAKE-CONDITION names a compiled function:
  Lambda-list: (TYPE &REST INITARGS)
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS) &REST T)
                  (VALUES CONDITION &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES CONDITION &OPTIONAL))
  Documentation:
    Make an instance of a condition object using the specified initargs.
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `MAKE-DISPATCH-MACRO-CHARACTER`

```lisp
COMMON-LISP:MAKE-DISPATCH-MACRO-CHARACTER
  [symbol]

MAKE-DISPATCH-MACRO-CHARACTER names a compiled function:
  Lambda-list: (CHAR &OPTIONAL (NON-TERMINATING-P NIL) (RT *READTABLE*))
  Declared type: (FUNCTION (CHARACTER &OPTIONAL T READTABLE)
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Cause CHAR to become a dispatching macro character in readtable (which
       defaults to the current readtable). If NON-TERMINATING-P, the char will
       be non-terminating.
  Source file: SYS:SRC;CODE;READER.LISP

```

### `MAKE-ECHO-STREAM`

```lisp
COMMON-LISP:MAKE-ECHO-STREAM
  [symbol]

MAKE-ECHO-STREAM names a compiled function:
  Lambda-list: (INPUT-STREAM OUTPUT-STREAM)
  Declared type: (FUNCTION (STREAM STREAM)
                  (VALUES ECHO-STREAM &OPTIONAL))
  Documentation:
    Return a bidirectional stream which gets its input from INPUT-STREAM and
       sends its output to OUTPUT-STREAM. In addition, all input is echoed to
       the output stream.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `MAKE-HASH-TABLE`

```lisp
COMMON-LISP:MAKE-HASH-TABLE
  [symbol]

MAKE-HASH-TABLE names a compiled function:
  Lambda-list: (&KEY (TEST (QUOTE EQL)) (SIZE 7) (REHASH-SIZE 1.5)
                (REHASH-THRESHOLD 1) (HASH-FUNCTION NIL USER-HASHFUN-P)
                (WEAKNESS NIL) (SYNCHRONIZED))
  Declared type: (FUNCTION
                  (&KEY (:TEST (OR FUNCTION SYMBOL))
                   (:SIZE UNSIGNED-BYTE)
                   (:REHASH-SIZE (OR (FLOAT (1.0)) (INTEGER 1)))
                   (:REHASH-THRESHOLD (REAL 0 1))
                   (:HASH-FUNCTION (OR FUNCTION SYMBOL))
                   (:WEAKNESS
                    (MEMBER :KEY-OR-VALUE :KEY-AND-VALUE :VALUE :KEY
                            NIL))
                   (:SYNCHRONIZED T))
                  (VALUES HASH-TABLE &OPTIONAL))
  Documentation:
    Create and return a new hash table. The keywords are as follows:
    
      :TEST
        Determines how keys are compared. Must a designator for one of the
        standard hash table tests, or a hash table test defined using
        SB-EXT:DEFINE-HASH-TABLE-TEST. Additionally, when an explicit
        HASH-FUNCTION is provided (see below), any two argument equivalence
        predicate can be used as the TEST.
    
      :SIZE
        A hint as to how many elements will be put in this hash table.
    
      :REHASH-SIZE
        Indicates how to expand the table when it fills up. If an integer, add
        space for that many elements. If a floating point number (which must be
        greater than 1.0), multiply the size by that amount.
    
      :REHASH-THRESHOLD
        Indicates how dense the table can become before forcing a rehash. Can be
        any positive number <=1, with density approaching zero as the threshold
        approaches 0. Density 1 means an average of one entry per bucket.
    
      :HASH-FUNCTION
        If unsupplied, a hash function based on the TEST argument is used,
        which then must be one of the standardized hash table test functions, or
        one for which a default hash function has been defined using
        SB-EXT:DEFINE-HASH-TABLE-TEST. If HASH-FUNCTION is specified, the TEST
        argument can be any two argument predicate consistent with it. The
        HASH-FUNCTION is expected to return a non-negative fixnum hash code.
        If TEST is neither standard nor defined by DEFINE-HASH-TABLE-TEST,
        then the HASH-FUNCTION must be specified.
    
      :WEAKNESS
        When :WEAKNESS is not NIL, garbage collection may remove entries from the
        hash table. The value of :WEAKNESS specifies how the presence of a key or
        value in the hash table preserves their entries from garbage collection.
    
        Valid values are:
    
          :KEY means that the key of an entry must be live to guarantee that the
            entry is preserved.
    
          :VALUE means that the value of an entry must be live to guarantee that
            the entry is preserved.
    
          :KEY-AND-VALUE means that both the key and the value must be live to
            guarantee that the entry is preserved.
    
          :KEY-OR-VALUE means that either the key or the value must be live to
            guarantee that the entry is preserved.
    
          NIL (the default) means that entries are always preserved.
    
      :SYNCHRONIZED
        If NIL (the default), the hash-table may have multiple concurrent readers,
        but results are undefined if a thread writes to the hash-table
        concurrently with another reader or writer. If T, all concurrent accesses
        are safe, but note that CLHS 3.6 (Traversal Rules and Side Effects)
        remains in force. See also: SB-EXT:WITH-LOCKED-HASH-TABLE.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-HASH-TABLE.LISP

MAKE-HASH-TABLE has a compiler-macro:
  Source file: SYS:SRC;CODE;MAPHASH.LISP

```

### `MAKE-LIST`

```lisp
COMMON-LISP:MAKE-LIST
  [symbol]

MAKE-LIST names a compiled function:
  Lambda-list: (SIZE &KEY INITIAL-ELEMENT)
  Declared type: (FUNCTION
                  ((UNSIGNED-BYTE 58) &KEY (:INITIAL-ELEMENT T))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T &KEY (:INITIAL-ELEMENT T))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Construct and return a list with SIZE elements each set to INITIAL-ELEMENT.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MAKE-LOAD-FORM-SAVING-SLOTS`

```lisp
COMMON-LISP:MAKE-LOAD-FORM-SAVING-SLOTS
  [symbol]

MAKE-LOAD-FORM-SAVING-SLOTS names a compiled function:
  Lambda-list: (OBJECT &KEY (SLOT-NAMES NIL SLOT-NAMES-P) ENVIRONMENT)
  Derived type: (FUNCTION (T &KEY (:SLOT-NAMES T) (:ENVIRONMENT T))
                 (VALUES CONS CONS &OPTIONAL))
  Source file: SYS:SRC;CODE;DEFSTRUCT.LISP

```

### `MAKE-PACKAGE`

```lisp
COMMON-LISP:MAKE-PACKAGE
  [symbol]

MAKE-PACKAGE names a compiled function:
  Lambda-list: (NAME &KEY (USE (QUOTE NIL)) NICKNAMES
                (INTERNAL-SYMBOLS 10) (EXTERNAL-SYMBOLS 10))
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER) &KEY (:USE LIST)
                   (:NICKNAMES LIST)
                   (:INTERNAL-SYMBOLS (UNSIGNED-BYTE 45))
                   (:EXTERNAL-SYMBOLS (UNSIGNED-BYTE 45)))
                  (VALUES PACKAGE &OPTIONAL))
  Documentation:
    Make a new package having the specified NAME, NICKNAMES, and USE
    list. :INTERNAL-SYMBOLS and :EXTERNAL-SYMBOLS are estimates for the number of
    internal and external symbols which will ultimately be present in the package.
    The default value of USE is implementation-dependent, and in this
    implementation it is NIL.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;DEFPACKAGE.LISP

```

### `MAKE-PATHNAME`

```lisp
COMMON-LISP:MAKE-PATHNAME
  [symbol]

MAKE-PATHNAME names a compiled function:
  Lambda-list: (&KEY HOST (DEVICE NIL DEVP) (DIRECTORY NIL DIRP)
                (NAME NIL NAMEP) (TYPE NIL TYPEP)
                (VERSION NIL VERSIONP) DEFAULTS (CASE LOCAL))
  Declared type: (FUNCTION
                  (&KEY
                   (:DEFAULTS
                    (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                   (:HOST (OR STRING SB-KERNEL:HOST NULL))
                   (:DEVICE (OR STRING (MEMBER :UNC :UNSPECIFIC NIL)))
                   (:DIRECTORY (OR STRING CONS (MEMBER :WILD NIL)))
                   (:NAME
                    (OR STRING SB-IMPL::PATTERN
                        (MEMBER . #1=(:WILD :UNSPECIFIC NIL))))
                   (:TYPE (OR STRING SB-IMPL::PATTERN (MEMBER . #1#)))
                   (:VERSION
                    (OR INTEGER
                        (MEMBER :UNSPECIFIC :WILD :NEWEST NIL)))
                   (:CASE (MEMBER :COMMON :LOCAL)))
                  (VALUES PATHNAME &OPTIONAL))
  Derived type: (FUNCTION
                 (&KEY (:HOST (OR STRING SB-KERNEL:HOST . #1=(NULL)))
                  (:DEVICE
                   (OR STRING
                       (MEMBER :UNC . #2=(:UNSPECIFIC . #3=(NIL)))))
                  (:DIRECTORY (OR STRING CONS (MEMBER :WILD . #3#)))
                  (:NAME
                   . #4=((OR STRING SB-IMPL::PATTERN
                             (MEMBER :WILD . #2#))))
                  (:TYPE . #4#)
                  (:VERSION
                   (OR INTEGER
                       (MEMBER :UNSPECIFIC :WILD :NEWEST . #3#)))
                  (:DEFAULTS
                   (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM
                       . #1#))
                  (:CASE (MEMBER :COMMON :LOCAL)))
                 (VALUES PATHNAME &OPTIONAL))
  Documentation:
    Makes a new pathname from the component arguments. Note that host is
    a host-structure or string.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `MAKE-RANDOM-STATE`

```lisp
COMMON-LISP:MAKE-RANDOM-STATE
  [symbol]

MAKE-RANDOM-STATE names a compiled function:
  Lambda-list: (&OPTIONAL STATE)
  Declared type: (FUNCTION (&OPTIONAL (OR RANDOM-STATE BOOLEAN))
                  (VALUES RANDOM-STATE &OPTIONAL))
  Documentation:
    Make a random state object. The optional STATE argument specifies a seed
    for deterministic pseudo-random number generation.
    
    As per the Common Lisp standard,
    - If STATE is NIL or not supplied, return a copy of the default
      *RANDOM-STATE*.
    - If STATE is a random state, return a copy of it.
    - If STATE is T, return a randomly initialized state (using operating-system
      provided randomness where available, otherwise a poor substitute based on
      internal time and PID).
    
    See SB-EXT:SEED-RANDOM-STATE for a SBCL extension to this functionality.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-RANDOM.LISP

```

### `MAKE-SEQUENCE`

```lisp
COMMON-LISP:MAKE-SEQUENCE
  [symbol]

MAKE-SEQUENCE names a compiled function:
  Lambda-list: (RESULT-TYPE LENGTH &KEY (INITIAL-ELEMENT NIL IEP))
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS)
                   (UNSIGNED-BYTE 45) &KEY (:INITIAL-ELEMENT T))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (T (UNSIGNED-BYTE 45) &KEY (:INITIAL-ELEMENT T))
                 (VALUES
                  (OR LIST (SIMPLE-ARRAY * (*))
                      SB-KERNEL:EXTENDED-SEQUENCE)
                  &OPTIONAL))
  Documentation:
    Return a sequence of the given RESULT-TYPE and LENGTH, with
      elements initialized to INITIAL-ELEMENT.
  Known attributes: movable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `MAKE-STRING`

```lisp
COMMON-LISP:MAKE-STRING
  [symbol]

MAKE-STRING names a compiled function:
  Lambda-list: (COUNT &KEY (ELEMENT-TYPE (QUOTE CHARACTER))
                      (INITIAL-ELEMENT NIL IEP))
  Declared type: (FUNCTION
                  ((UNSIGNED-BYTE 45) &KEY
                   (:ELEMENT-TYPE
                    (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS))
                   (:INITIAL-ELEMENT CHARACTER))
                  (VALUES SIMPLE-STRING &OPTIONAL))
  Derived type: (FUNCTION
                 ((UNSIGNED-BYTE 45) &KEY (:ELEMENT-TYPE . #1=(T))
                  (:INITIAL-ELEMENT . #1#))
                 (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
  Documentation:
    Given a character count and an optional fill character, makes and returns a
    new string COUNT long filled with the fill character.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `MAKE-STRING-INPUT-STREAM`

```lisp
COMMON-LISP:MAKE-STRING-INPUT-STREAM
  [symbol]

MAKE-STRING-INPUT-STREAM names a compiled function:
  Lambda-list: (STRING &OPTIONAL (START 0) END)
  Declared type: (FUNCTION
                  (STRING &OPTIONAL (UNSIGNED-BYTE 45)
                          (OR NULL (UNSIGNED-BYTE 45)))
                  (VALUES SB-IMPL::STRING-INPUT-STREAM &OPTIONAL))
  Documentation:
    Return an input stream which will supply the characters of STRING between
      START and END in order.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `MAKE-STRING-OUTPUT-STREAM`

```lisp
COMMON-LISP:MAKE-STRING-OUTPUT-STREAM
  [symbol]

MAKE-STRING-OUTPUT-STREAM names a compiled function:
  Lambda-list: (&KEY (ELEMENT-TYPE (QUOTE CHARACTER)))
  Declared type: (FUNCTION
                  (&KEY
                   (:ELEMENT-TYPE
                    (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS)))
                  (VALUES SB-IMPL::STRING-OUTPUT-STREAM &OPTIONAL))
  Derived type: (FUNCTION (&KEY (:ELEMENT-TYPE T))
                 (VALUES SB-IMPL::STRING-OUTPUT-STREAM &OPTIONAL))
  Documentation:
    Return an output stream which will accumulate all output given it for the
    benefit of the function GET-OUTPUT-STREAM-STRING.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `MAKE-SYMBOL`

```lisp
COMMON-LISP:MAKE-SYMBOL
  [symbol]

MAKE-SYMBOL names a compiled function:
  Lambda-list: (STRING)
  Declared type: (FUNCTION (STRING) (VALUES SYMBOL &OPTIONAL))
  Documentation:
    Make and return a new symbol with the STRING as its print name.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `MAKE-SYNONYM-STREAM`

```lisp
COMMON-LISP:MAKE-SYNONYM-STREAM
  [symbol]

MAKE-SYNONYM-STREAM names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL) (VALUES SYNONYM-STREAM &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES SYNONYM-STREAM &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ANSI-STREAM.LISP

```

### `MAKE-TWO-WAY-STREAM`

```lisp
COMMON-LISP:MAKE-TWO-WAY-STREAM
  [symbol]

MAKE-TWO-WAY-STREAM names a compiled function:
  Lambda-list: (INPUT-STREAM OUTPUT-STREAM)
  Declared type: (FUNCTION (STREAM STREAM)
                  (VALUES TWO-WAY-STREAM &OPTIONAL))
  Documentation:
    Return a bidirectional stream which gets its input from INPUT-STREAM and
       sends its output to OUTPUT-STREAM.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `MAKUNBOUND`

```lisp
COMMON-LISP:MAKUNBOUND
  [symbol]

MAKUNBOUND names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL) (VALUES SYMBOL &OPTIONAL))
  Documentation:
    Make SYMBOL unbound, removing any value it may currently have.
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `MAP`

```lisp
COMMON-LISP:MAP
  [symbol]

MAP names a compiled function:
  Lambda-list: (RESULT-TYPE FUNCTION FIRST-SEQUENCE &REST
                MORE-SEQUENCES)
  Dynamic-extent arguments: positional=(1)
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS)
                   (OR FUNCTION SYMBOL) SEQUENCE &REST SEQUENCE)
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION (T T T &REST T)
                 (VALUES
                  (OR LIST (SIMPLE-ARRAY * (*))
                      SB-KERNEL:EXTENDED-SEQUENCE)
                  &OPTIONAL))
  Known attributes: call
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `MAP-INTO`

```lisp
COMMON-LISP:MAP-INTO
  [symbol]

MAP-INTO names a compiled function:
  Lambda-list: (RESULT-SEQUENCE FUNCTION &REST SEQUENCES)
  Dynamic-extent arguments: positional=(1)
  Declared type: (FUNCTION
                  (SEQUENCE (OR FUNCTION SYMBOL) &REST SEQUENCE)
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION (T T &REST T) (VALUES T &OPTIONAL))
  Known attributes: call
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `MAPC`

```lisp
COMMON-LISP:MAPC
  [symbol]

MAPC names a compiled function:
  Lambda-list: (FUNCTION LIST &REST MORE-LISTS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) LIST &REST LIST)
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
    Return LIST.
  Known attributes: call, foldable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MAPCAN`

```lisp
COMMON-LISP:MAPCAN
  [symbol]

MAPCAN names a compiled function:
  Lambda-list: (FUNCTION LIST &REST MORE-LISTS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) LIST &REST LIST)
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
    Return NCONC of FUNCTION return values.
  Known attributes: call
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MAPCAR`

```lisp
COMMON-LISP:MAPCAR
  [symbol]

MAPCAR names a compiled function:
  Lambda-list: (FUNCTION LIST &REST MORE-LISTS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) LIST &REST LIST)
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
    Return list of FUNCTION return values.
  Known attributes: call
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MAPCON`

```lisp
COMMON-LISP:MAPCON
  [symbol]

MAPCON names a compiled function:
  Lambda-list: (FUNCTION LIST &REST MORE-LISTS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) LIST &REST LIST)
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Apply FUNCTION to successive tuples of CDRs of LIST and MORE-LISTS.
    Return NCONC of results.
  Known attributes: call
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MAPHASH`

```lisp
COMMON-LISP:MAPHASH
  [symbol]

MAPHASH names a compiled function:
  Lambda-list: (FUNCTION-DESIGNATOR HASH-TABLE)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) HASH-TABLE)
                  (VALUES NULL &OPTIONAL))
  Documentation:
    For each entry in HASH-TABLE, call the designated two-argument function on
    the key and value of the entry. Return NIL.
    
    Consequences are undefined if HASH-TABLE is mutated during the call to
    MAPHASH, except for changing or removing elements corresponding to the
    current key. The applies to all threads, not just the current one --
    even for synchronized hash-tables. If the table may be mutated by
    another thread during iteration, use eg. SB-EXT:WITH-LOCKED-HASH-TABLE
    to protect the MAPHASH call.
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;MAPHASH.LISP

MAPHASH has a compiler-macro:
  Source file: SYS:SRC;CODE;MAPHASH.LISP

```

### `MAPL`

```lisp
COMMON-LISP:MAPL
  [symbol]

MAPL names a compiled function:
  Lambda-list: (FUNCTION LIST &REST MORE-LISTS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) LIST &REST LIST)
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Apply FUNCTION to successive tuples of CDRs of LIST and MORE-LISTS.
    Return LIST.
  Known attributes: call, foldable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MAPLIST`

```lisp
COMMON-LISP:MAPLIST
  [symbol]

MAPLIST names a compiled function:
  Lambda-list: (FUNCTION LIST &REST MORE-LISTS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION ((OR FUNCTION SYMBOL) LIST &REST LIST)
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &REST T) (VALUES T &OPTIONAL))
  Documentation:
    Apply FUNCTION to successive tuples of CDRs of LIST and MORE-LISTS.
    Return list of results.
  Known attributes: call
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MASK-FIELD`

```lisp
COMMON-LISP:MASK-FIELD
  [symbol]

MASK-FIELD names a compiled function:
  Lambda-list: (BYTESPEC INTEGER)
  Declared type: (FUNCTION (CONS INTEGER)
                  (VALUES UNSIGNED-BYTE &OPTIONAL))
  Documentation:
    Extract the specified byte from integer,  but do not right justify result.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

(SETF MASK-FIELD) has a complex setf-expansion:
  Lambda-list: (&REST ARGS)
  Documentation:
    The first argument is a byte specifier. The second is any place form
    acceptable to SETF. Replaces the specified byte of the number in this place
    with bits from the corresponding position in the new value.
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `MAX`

```lisp
COMMON-LISP:MAX
  [symbol]

MAX names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (REAL &REST REAL) (VALUES REAL &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES REAL &OPTIONAL))
  Documentation:
    Return the greatest of its arguments; among EQUALP greatest, return
    the first.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `MEMBER`

```lisp
COMMON-LISP:MEMBER
  [symbol]

MEMBER names a compiled function:
  Lambda-list: (ITEM LIST &KEY KEY (TEST NIL TESTP) (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Return the tail of LIST beginning with first element satisfying EQLity,
       :TEST, or :TEST-NOT with the given ITEM.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MEMBER-IF`

```lisp
COMMON-LISP:MEMBER-IF
  [symbol]

MEMBER-IF names a compiled function:
  Lambda-list: (TEST LIST &KEY KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) LIST &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &KEY (:KEY T)) (VALUES LIST &OPTIONAL))
  Documentation:
    Return tail of LIST beginning with first element satisfying TEST.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MEMBER-IF-NOT`

```lisp
COMMON-LISP:MEMBER-IF-NOT
  [symbol]

MEMBER-IF-NOT names a compiled function:
  Lambda-list: (TEST LIST &KEY KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) LIST &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &KEY (:KEY T)) (VALUES LIST &OPTIONAL))
  Documentation:
    Return tail of LIST beginning with first element not satisfying TEST.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `MERGE`

```lisp
COMMON-LISP:MERGE
  [symbol]

MERGE names a compiled function:
  Lambda-list: (RESULT-TYPE SEQUENCE1 SEQUENCE2 PREDICATE &KEY KEY)
  Dynamic-extent arguments: positional=(3), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS) SEQUENCE
                   SEQUENCE (OR FUNCTION SYMBOL) &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION (T T T T &KEY (:KEY T)) (VALUES T &OPTIONAL))
  Documentation:
    Merge the sequences SEQUENCE1 and SEQUENCE2 destructively into a
       sequence of type RESULT-TYPE using PREDICATE to order the elements.
  Known attributes: call, important-result
  Source file: SYS:SRC;CODE;SORT.LISP

```

### `MERGE-PATHNAMES`

```lisp
COMMON-LISP:MERGE-PATHNAMES
  [symbol]

MERGE-PATHNAMES names a compiled function:
  Lambda-list: (PATHNAME &OPTIONAL
                         (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
                         (DEFAULT-VERSION NEWEST))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   &OPTIONAL
                   (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   (OR INTEGER (MEMBER :UNSPECIFIC :WILD :NEWEST NIL)))
                  (VALUES PATHNAME &OPTIONAL))
  Documentation:
    Construct a filled in pathname by completing the unspecified components
       from the defaults.
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `METHOD-COMBINATION-ERROR`

```lisp
COMMON-LISP:METHOD-COMBINATION-ERROR
  [symbol]

METHOD-COMBINATION-ERROR names a compiled function:
  Lambda-list: (FORMAT-CONTROL &REST FORMAT-ARGUMENTS)
  Declared type: (FUNCTION ((OR STRING FUNCTION) &REST T) *)
  Derived type: (FUNCTION ((OR STRING FUNCTION) &REST T) NIL)
  Known attributes: unwind, any
  Source file: SYS:SRC;PCL;COMBIN.LISP

```

### `MIN`

```lisp
COMMON-LISP:MIN
  [symbol]

MIN names a compiled function:
  Lambda-list: (NUMBER &REST MORE-NUMBERS)
  Declared type: (FUNCTION (REAL &REST REAL) (VALUES REAL &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) (VALUES REAL &OPTIONAL))
  Documentation:
    Return the least of its arguments; among EQUALP least, return
    the first.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `MINUSP`

```lisp
COMMON-LISP:MINUSP
  [symbol]

MINUSP names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (REAL) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Is this real number strictly negative?
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `MISMATCH`

```lisp
COMMON-LISP:MISMATCH
  [symbol]

MISMATCH names a compiled function:
  Lambda-list: (SEQUENCE1 SEQUENCE2 &REST ARGS &KEY FROM-END
                (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START1 0)
                (END1 NIL) (START2 0) (END2 NIL) (KEY NIL))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT :KEY)
  Declared type: (FUNCTION
                  (SEQUENCE SEQUENCE &REST T &KEY (:FROM-END T)
                   (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &REST T &KEY (:FROM-END T)
                  (:TEST . #1=((OR FUNCTION SYMBOL))) (:TEST-NOT . #1#)
                  (:START1 . #2=((UNSIGNED-BYTE 45)))
                  (:END1 . #3=((OR NULL . #2#))) (:START2 . #2#)
                  (:END2 . #3#) (:KEY . #1#))
                 (VALUES
                  (OR
                   (INTEGER -4611686018427387904 4611686018427387904)
                   NULL)
                  &OPTIONAL))
  Documentation:
    The specified subsequences of SEQUENCE1 and SEQUENCE2 are compared
       element-wise. If they are of equal length and match in every element, the
       result is NIL. Otherwise, the result is a non-negative integer, the index
       within SEQUENCE1 of the leftmost position at which they fail to match; or,
       if one is shorter than and a matching prefix of the other, the index within
       SEQUENCE1 beyond the last position tested is returned. If a non-NIL
       :FROM-END argument is given, then one plus the index of the rightmost
       position in which the sequences differ is returned.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `MOD`

```lisp
COMMON-LISP:MOD
  [symbol]

MOD names a compiled function:
  Lambda-list: (NUMBER DIVISOR)
  Declared type: (FUNCTION (REAL REAL) (VALUES REAL &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES REAL &OPTIONAL))
  Documentation:
    Return second result of FLOOR.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

MOD names a type-specifier:
  Lambda-list: (N)

```

### `MUFFLE-WARNING`

```lisp
COMMON-LISP:MUFFLE-WARNING
  [symbol]

MUFFLE-WARNING names a compiled function:
  Lambda-list: (&OPTIONAL CONDITION)
  Declared type: (FUNCTION (&OPTIONAL (OR CONDITION NULL)) NIL)
  Documentation:
    Transfer control to a restart named MUFFLE-WARNING, signalling a
       CONTROL-ERROR if none exists.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `NAME-CHAR`

```lisp
COMMON-LISP:NAME-CHAR
  [symbol]

NAME-CHAR names a compiled function:
  Lambda-list: (NAME)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER))
                  (VALUES (OR CHARACTER NULL) &OPTIONAL))
  Documentation:
    Given an argument acceptable to STRING, NAME-CHAR returns a character whose
    name is that string, if one exists. Otherwise, NIL is returned.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-UNICODE.LISP

```

### `NAMESTRING`

```lisp
COMMON-LISP:NAMESTRING
  [symbol]

NAMESTRING names a compiled function:
  Lambda-list: (PATHNAME)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Construct the full (name)string form PATHNAME.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `NBUTLAST`

```lisp
COMMON-LISP:NBUTLAST
  [symbol]

NBUTLAST names a compiled function:
  Lambda-list: (LIST &OPTIONAL (N 1))
  Declared type: (FUNCTION (LIST &OPTIONAL UNSIGNED-BYTE)
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (LIST &OPTIONAL T) (VALUES LIST &OPTIONAL))
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NCONC`

```lisp
COMMON-LISP:NCONC
  [symbol]

NCONC names a compiled function:
  Lambda-list: (&REST LISTS)
  Declared type: (FUNCTION * (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES T &OPTIONAL))
  Documentation:
    Concatenates the lists given as arguments (by changing them)
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NINTERSECTION`

```lisp
COMMON-LISP:NINTERSECTION
  [symbol]

NINTERSECTION names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Destructively return the intersection of LIST1 and LIST2.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, foldable, flushable, unsafely-flushable, important-result
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NINTH`

```lisp
COMMON-LISP:NINTH
  [symbol]

NINTH names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 9th object in a list or NIL if there is no 9th object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF NINTH) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF NINTH) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF NINTH)`

```lisp
COMMON-LISP-USER::|(SETF NINTH)|
  [symbol]

```

### `NOT`

```lisp
COMMON-LISP:NOT
  [symbol]

NOT names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if X is NIL, otherwise return NIL.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;PRED.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION NOT(..

```

### `NOTANY`

```lisp
COMMON-LISP:NOTANY
  [symbol]

NOTANY names a compiled function:
  Lambda-list: (PRED FIRST-SEQ &REST MORE-SEQS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST SEQUENCE)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION ((OR FUNCTION SYMBOL) SEQUENCE &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Apply PREDICATE to the 0-indexed elements of the sequences, then
       possibly to those with index 1, and so on. Return NIL as soon
       as any invocation of PREDICATE returns a non-NIL value, or T if the end
       of any sequence is reached.
  Known attributes: call, foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;QUANTIFIERS.LISP

```

### `NOTEVERY`

```lisp
COMMON-LISP:NOTEVERY
  [symbol]

NOTEVERY names a compiled function:
  Lambda-list: (PRED FIRST-SEQ &REST MORE-SEQS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST SEQUENCE)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION ((OR FUNCTION SYMBOL) SEQUENCE &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Apply PREDICATE to 0-indexed elements of the sequences, then
       possibly to those with index 1, and so on. Return T as soon
       as any invocation of PREDICATE returns NIL, or NIL if every invocation
       is non-NIL.
  Known attributes: call, foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;QUANTIFIERS.LISP

```

### `NRECONC`

```lisp
COMMON-LISP:NRECONC
  [symbol]

NRECONC names a compiled function:
  Lambda-list: (X Y)
  Declared type: (FUNCTION (LIST T) (VALUES T &OPTIONAL))
  Documentation:
    Return (NCONC (NREVERSE X) Y).
  Known attributes: important-result
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NREVERSE`

```lisp
COMMON-LISP:NREVERSE
  [symbol]

NREVERSE names a compiled function:
  Lambda-list: (SEQUENCE)
  Declared type: (FUNCTION (SEQUENCE) (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES SEQUENCE &OPTIONAL))
  Documentation:
    Return a sequence of the same elements in reverse order; the argument
       is destroyed.
  Known attributes: important-result
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `NSET-DIFFERENCE`

```lisp
COMMON-LISP:NSET-DIFFERENCE
  [symbol]

NSET-DIFFERENCE names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Destructively return the elements of LIST1 which are not in LIST2.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, foldable, flushable, unsafely-flushable, important-result
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NSET-EXCLUSIVE-OR`

```lisp
COMMON-LISP:NSET-EXCLUSIVE-OR
  [symbol]

NSET-EXCLUSIVE-OR names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP)
                (TEST-NOT (FUNCTION EQL) NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Destructively return a list with elements which appear but once in LIST1
       and LIST2.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, foldable, flushable, unsafely-flushable, important-result
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NSTRING-CAPITALIZE`

```lisp
COMMON-LISP:NSTRING-CAPITALIZE
  [symbol]

NSTRING-CAPITALIZE names a compiled function:
  Lambda-list: (STRING &KEY (START 0) END)
  Declared type: (FUNCTION
                  (STRING &KEY (:START (UNSIGNED-BYTE 45))
                          (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES STRING &OPTIONAL))
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `NSTRING-DOWNCASE`

```lisp
COMMON-LISP:NSTRING-DOWNCASE
  [symbol]

NSTRING-DOWNCASE names a compiled function:
  Lambda-list: (STRING &KEY (START 0) END)
  Declared type: (FUNCTION
                  (STRING &KEY (:START (UNSIGNED-BYTE 45))
                          (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES STRING &OPTIONAL))
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `NSTRING-UPCASE`

```lisp
COMMON-LISP:NSTRING-UPCASE
  [symbol]

NSTRING-UPCASE names a compiled function:
  Lambda-list: (STRING &KEY (START 0) END)
  Declared type: (FUNCTION
                  (STRING &KEY (:START (UNSIGNED-BYTE 45))
                          (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES STRING &OPTIONAL))
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `NSUBLIS`

```lisp
COMMON-LISP:NSUBLIS
  [symbol]

NSUBLIS names a compiled function:
  Lambda-list: (ALIST TREE &KEY KEY (TEST (FUNCTION EQL) TESTP)
                (TEST-NOT (FUNCTION EQL) NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST T &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Documentation:
    Substitute from ALIST into TREE destructively.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NSUBST`

```lisp
COMMON-LISP:NSUBST
  [symbol]

NSUBST names a compiled function:
  Lambda-list: (NEW OLD TREE &KEY KEY (TEST (FUNCTION EQL) TESTP)
                (TEST-NOT (FUNCTION EQL) NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T T T &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Documentation:
    Substitute NEW for subtrees matching OLD.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NSUBST-IF`

```lisp
COMMON-LISP:NSUBST-IF
  [symbol]

NSUBST-IF names a compiled function:
  Lambda-list: (NEW TEST TREE &KEY KEY)
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (T (OR FUNCTION SYMBOL) T &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Documentation:
    Substitute NEW for subtrees of TREE for which TEST is true.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NSUBST-IF-NOT`

```lisp
COMMON-LISP:NSUBST-IF-NOT
  [symbol]

NSUBST-IF-NOT names a compiled function:
  Lambda-list: (NEW TEST TREE &KEY KEY)
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (T (OR FUNCTION SYMBOL) T &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Documentation:
    Substitute NEW for subtrees of TREE for which TEST is false.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NSUBSTITUTE`

```lisp
COMMON-LISP:NSUBSTITUTE
  [symbol]

NSUBSTITUTE names a compiled function:
  Lambda-list: (NEW OLD SEQUENCE &REST ARGS &KEY FROM-END
                (TEST (FUNCTION EQL)) (TEST-NOT NIL) (END NIL)
                (COUNT NIL) (KEY NIL) (START 0))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT :KEY)
  Declared type: (FUNCTION
                  (T T SEQUENCE &REST T &KEY (:FROM-END T)
                   (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:COUNT (OR NULL INTEGER))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (T T T &REST T &KEY (:FROM-END T)
                  (:TEST . #1=((OR FUNCTION SYMBOL))) (:TEST-NOT . #1#)
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:COUNT (OR NULL INTEGER))
                  (:KEY . #1#))
                 *)
  Documentation:
    Return a sequence of the same kind as SEQUENCE with the same elements
      except that all elements equal to OLD are replaced with NEW. SEQUENCE
      may be destructively modified.
  Known attributes: call
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `NSUBSTITUTE-IF`

```lisp
COMMON-LISP:NSUBSTITUTE-IF
  [symbol]

NSUBSTITUTE-IF names a compiled function:
  Lambda-list: (NEW PREDICATE SEQUENCE &REST ARGS &KEY FROM-END
                (START 0) (END NIL) (COUNT NIL) (KEY NIL))
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (T (OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:COUNT (OR NULL INTEGER))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (T #1=(OR FUNCTION SYMBOL) T &REST T &KEY
                  (:FROM-END T) (:COUNT (OR NULL INTEGER))
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 *)
  Documentation:
    Return a sequence of the same kind as SEQUENCE with the same elements
       except that all elements satisfying PREDICATE are replaced with NEW.
       SEQUENCE may be destructively modified.
  Known attributes: call
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `NSUBSTITUTE-IF-NOT`

```lisp
COMMON-LISP:NSUBSTITUTE-IF-NOT
  [symbol]

NSUBSTITUTE-IF-NOT names a compiled function:
  Lambda-list: (NEW PREDICATE SEQUENCE &REST ARGS &KEY FROM-END
                (START 0) (END NIL) (COUNT NIL) (KEY NIL))
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (T (OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:COUNT (OR NULL INTEGER))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (T #1=(OR FUNCTION SYMBOL) T &REST T &KEY
                  (:FROM-END T) (:COUNT (OR NULL INTEGER))
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 *)
  Documentation:
    Return a sequence of the same kind as SEQUENCE with the same elements
       except that all elements not satisfying PREDICATE are replaced with NEW.
       SEQUENCE may be destructively modified.
  Known attributes: call
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `NTH`

```lisp
COMMON-LISP:NTH
  [symbol]

NTH names a compiled function:
  Lambda-list: (N LIST)
  Declared type: (FUNCTION (UNSIGNED-BYTE LIST) (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES T &OPTIONAL))
  Documentation:
    Return the nth object in a list where the car is the zero-th element.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF NTH) names a compiled function:
  Lambda-list: (NEWVAL N LIST)
  Derived type: (FUNCTION (T UNSIGNED-BYTE LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF NTH) has a complex setf-expansion:
  Lambda-list: (N LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF NTH)`

```lisp
COMMON-LISP-USER::|(SETF NTH)|
  [symbol]

```

### `NTHCDR`

```lisp
COMMON-LISP:NTHCDR
  [symbol]

NTHCDR names a compiled function:
  Lambda-list: (N LIST)
  Declared type: (FUNCTION (UNSIGNED-BYTE LIST) (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Documentation:
    Performs the cdr function n times on a list.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `NULL`

```lisp
COMMON-LISP:NULL
  [symbol]

NULL names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a NULL, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;PRED.LISP

NULL names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:NULL>:
  Class precedence-list: NULL, SYMBOL, LIST, SEQUENCE, T
  Direct superclasses: SYMBOL, LIST
  No subclasses.
  Sealed.
  No direct slots.

```

### `NUMBERP`

```lisp
COMMON-LISP:NUMBERP
  [symbol]

NUMBERP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a NUMBER, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `NUMERATOR`

```lisp
COMMON-LISP:NUMERATOR
  [symbol]

NUMERATOR names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (RATIONAL) (VALUES INTEGER &OPTIONAL))
  Documentation:
    Return the numerator of NUMBER, which must be rational.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `NUNION`

```lisp
COMMON-LISP:NUNION
  [symbol]

NUNION names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Destructively return the union of LIST1 and LIST2.
  Known attributes: call, foldable, flushable, unsafely-flushable, important-result
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `ODDP`

```lisp
COMMON-LISP:ODDP
  [symbol]

ODDP names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (INTEGER) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Is this integer odd?
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `OPEN`

```lisp
COMMON-LISP:OPEN
  [symbol]

OPEN names a compiled function:
  Lambda-list: (FILENAME &KEY (DIRECTION INPUT)
                (ELEMENT-TYPE (QUOTE CHARACTER))
                (IF-EXISTS NIL IF-EXISTS-GIVEN)
                (IF-DOES-NOT-EXIST NIL IF-DOES-NOT-EXIST-GIVEN)
                (EXTERNAL-FORMAT DEFAULT) (CLASS (QUOTE FD-STREAM))
                (OVERLAPPED T))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:CLASS SYMBOL)
                   (:DIRECTION (MEMBER :INPUT :OUTPUT :IO :PROBE))
                   (:ELEMENT-TYPE
                    (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS))
                   (:IF-EXISTS
                    (MEMBER :ERROR :NEW-VERSION :RENAME
                            :RENAME-AND-DELETE :OVERWRITE :APPEND
                            :SUPERSEDE NIL))
                   (:IF-DOES-NOT-EXIST (MEMBER :ERROR :CREATE NIL))
                   (:EXTERNAL-FORMAT (OR KEYWORD (CONS KEYWORD T)))
                   (:OVERLAPPED T))
                  (VALUES (OR STREAM NULL) &OPTIONAL))
  Derived type: (FUNCTION
                 ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                  (:CLASS SYMBOL)
                  (:DIRECTION (MEMBER :PROBE :IO :OUTPUT :INPUT))
                  (:ELEMENT-TYPE
                   (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS))
                  (:IF-EXISTS
                   (MEMBER NIL :SUPERSEDE :APPEND :OVERWRITE
                           :RENAME-AND-DELETE :RENAME :NEW-VERSION
                           . #1=(:ERROR)))
                  (:IF-DOES-NOT-EXIST (MEMBER NIL :CREATE . #1#))
                  (:EXTERNAL-FORMAT
                   (OR KEYWORD (CONS KEYWORD . #2=(T))))
                  (:OVERLAPPED . #2#))
                 (VALUES (OR NULL SB-SYS:FD-STREAM) &OPTIONAL))
  Documentation:
    Return a stream which reads from or writes to FILENAME.
      Defined keywords:
       :DIRECTION - one of :INPUT, :OUTPUT, :IO, or :PROBE
       :ELEMENT-TYPE - the type of object to read or write, default BASE-CHAR
       :IF-EXISTS - one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE,
                           :OVERWRITE, :APPEND, :SUPERSEDE or NIL
       :IF-DOES-NOT-EXIST - one of :ERROR, :CREATE or NIL
      See the manual for details.
  Inline proclamation: NOTINLINE (no inline expansion available)
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;FD-STREAM.LISP

```

### `PACKAGE-ERROR-PACKAGE`

```lisp
COMMON-LISP:PACKAGE-ERROR-PACKAGE
  [symbol]

PACKAGE-ERROR-PACKAGE names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `PACKAGE-NAME`

```lisp
COMMON-LISP:PACKAGE-NAME
  [symbol]

PACKAGE-NAME names a compiled function:
  Lambda-list: (PACKAGE-DESIGNATOR)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `PACKAGE-NICKNAMES`

```lisp
COMMON-LISP:PACKAGE-NICKNAMES
  [symbol]

PACKAGE-NICKNAMES names a compiled function:
  Lambda-list: (PACKAGE-DESIGNATOR)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES LIST &OPTIONAL))
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `PACKAGE-SHADOWING-SYMBOLS`

```lisp
COMMON-LISP:PACKAGE-SHADOWING-SYMBOLS
  [symbol]

PACKAGE-SHADOWING-SYMBOLS names a compiled function:
  Lambda-list: (PACKAGE-DESIGNATOR)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES LIST &OPTIONAL))
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `PACKAGE-USE-LIST`

```lisp
COMMON-LISP:PACKAGE-USE-LIST
  [symbol]

PACKAGE-USE-LIST names a compiled function:
  Lambda-list: (PACKAGE-DESIGNATOR)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES LIST &OPTIONAL))
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `PACKAGE-USED-BY-LIST`

```lisp
COMMON-LISP:PACKAGE-USED-BY-LIST
  [symbol]

PACKAGE-USED-BY-LIST names a compiled function:
  Lambda-list: (PACKAGE-DESIGNATOR)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES LIST &OPTIONAL))
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `PACKAGEP`

```lisp
COMMON-LISP:PACKAGEP
  [symbol]

PACKAGEP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;PACKAGE.LISP

```

### `PAIRLIS`

```lisp
COMMON-LISP:PAIRLIS
  [symbol]

PAIRLIS names a compiled function:
  Lambda-list: (KEYS DATA &OPTIONAL (ALIST (QUOTE NIL)))
  Declared type: (FUNCTION (T T &OPTIONAL T) (VALUES LIST &OPTIONAL))
  Documentation:
    Construct an association list from KEYS and DATA (adding to ALIST).
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `PARSE-INTEGER`

```lisp
COMMON-LISP:PARSE-INTEGER
  [symbol]

PARSE-INTEGER names a compiled function:
  Lambda-list: (STRING &KEY (START 0) END (RADIX 10) JUNK-ALLOWED)
  Declared type: (FUNCTION
                  (STRING &KEY (:START (UNSIGNED-BYTE 45))
                          (:END (OR NULL (UNSIGNED-BYTE 45)))
                          (:RADIX (INTEGER 2 36)) (:JUNK-ALLOWED T))
                  (VALUES (OR NULL INTEGER) (UNSIGNED-BYTE 45)
                          &OPTIONAL))
  Documentation:
    Examine the substring of string delimited by start and end
      (default to the beginning and end of the string)  It skips over
      whitespace characters and then tries to parse an integer. The
      radix parameter must be between 2 and 36.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;READER.LISP

```

### `PARSE-NAMESTRING`

```lisp
COMMON-LISP:PARSE-NAMESTRING
  [symbol]

PARSE-NAMESTRING names a compiled function:
  Lambda-list: (THING &OPTIONAL HOST
                (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*) &KEY (START 0)
                END JUNK-ALLOWED)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   &OPTIONAL
                   (OR STRING CONS SB-KERNEL:HOST
                       (MEMBER :UNSPECIFIC NIL))
                   (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:JUNK-ALLOWED T))
                  (VALUES (OR PATHNAME NULL)
                          (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                  &OPTIONAL
                  (OR STRING CONS (MEMBER :UNSPECIFIC NIL)
                      SB-KERNEL:HOST)
                  #1# &KEY (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END #3=(OR NULL . #2#)) (:JUNK-ALLOWED T))
                 (VALUES (OR PATHNAME NULL) #3# &OPTIONAL))
  Known attributes: recursive
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `PATHNAME`

```lisp
COMMON-LISP:PATHNAME
  [symbol]

PATHNAME names a compiled function:
  Lambda-list: (PATHSPEC)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES PATHNAME &OPTIONAL))
  Documentation:
    Convert PATHSPEC (a pathname designator) into a pathname.
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

PATHNAME names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:PATHNAME>:
  Class precedence-list: PATHNAME, T
  Direct superclasses: T
  Direct subclasses: LOGICAL-PATHNAME
  Sealed.
  No direct slots.

```

### `PATHNAME-DEVICE`

```lisp
COMMON-LISP:PATHNAME-DEVICE
  [symbol]

PATHNAME-DEVICE names a compiled function:
  Lambda-list: (PATHNAME &KEY (CASE LOCAL))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:CASE (MEMBER :COMMON :LOCAL)))
                  (VALUES
                   (OR SIMPLE-STRING (MEMBER :UNC :UNSPECIFIC NIL))
                   &OPTIONAL))
  Documentation:
    Return PATHNAME's device.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `PATHNAME-DIRECTORY`

```lisp
COMMON-LISP:PATHNAME-DIRECTORY
  [symbol]

PATHNAME-DIRECTORY names a compiled function:
  Lambda-list: (PATHNAME &KEY (CASE LOCAL))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:CASE (MEMBER :COMMON :LOCAL)))
                  (VALUES LIST &OPTIONAL))
  Documentation:
    Return PATHNAME's directory.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `PATHNAME-HOST`

```lisp
COMMON-LISP:PATHNAME-HOST
  [symbol]

PATHNAME-HOST names a compiled function:
  Lambda-list: (PATHNAME &KEY (CASE LOCAL))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:CASE (MEMBER :COMMON :LOCAL)))
                  (VALUES (OR SB-KERNEL:HOST NULL) &OPTIONAL))
  Documentation:
    Return PATHNAME's host.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `PATHNAME-MATCH-P`

```lisp
COMMON-LISP:PATHNAME-MATCH-P
  [symbol]

PATHNAME-MATCH-P names a compiled function:
  Lambda-list: (IN-PATHNAME IN-WILDNAME)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                  #1#)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Pathname matches the wildname template?
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `PATHNAME-NAME`

```lisp
COMMON-LISP:PATHNAME-NAME
  [symbol]

PATHNAME-NAME names a compiled function:
  Lambda-list: (PATHNAME &KEY (CASE LOCAL))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:CASE (MEMBER :COMMON :LOCAL)))
                  (VALUES
                   (OR SIMPLE-STRING SB-IMPL::PATTERN
                       (MEMBER :WILD :UNSPECIFIC NIL))
                   &OPTIONAL))
  Documentation:
    Return PATHNAME's name.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `PATHNAME-TYPE`

```lisp
COMMON-LISP:PATHNAME-TYPE
  [symbol]

PATHNAME-TYPE names a compiled function:
  Lambda-list: (PATHNAME &KEY (CASE LOCAL))
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM) &KEY
                   (:CASE (MEMBER :COMMON :LOCAL)))
                  (VALUES
                   (OR SIMPLE-STRING SB-IMPL::PATTERN
                       (MEMBER :WILD :UNSPECIFIC NIL))
                   &OPTIONAL))
  Documentation:
    Return PATHNAME's type.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `PATHNAME-VERSION`

```lisp
COMMON-LISP:PATHNAME-VERSION
  [symbol]

PATHNAME-VERSION names a compiled function:
  Lambda-list: (PATHNAME)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES
                   (OR INTEGER (MEMBER :UNSPECIFIC :WILD :NEWEST NIL))
                   &OPTIONAL))
  Documentation:
    Return PATHNAME's version.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `PATHNAMEP`

```lisp
COMMON-LISP:PATHNAMEP
  [symbol]

PATHNAMEP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;PATHNAME.LISP

```

### `PEEK-CHAR`

```lisp
COMMON-LISP:PEEK-CHAR
  [symbol]

PEEK-CHAR names a compiled function:
  Lambda-list: (&OPTIONAL (PEEK-TYPE NIL) (STREAM *STANDARD-INPUT*)
                (EOF-ERROR-P T) EOF-VALUE RECURSIVE-P)
  Declared type: (FUNCTION
                  (&OPTIONAL (OR CHARACTER BOOLEAN) (OR STREAM BOOLEAN)
                   T T T)
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL (OR CHARACTER BOOLEAN) T T T T) *)
  Source file: SYS:SRC;CODE;TARGET-STREAM.LISP

```

### `PHASE`

```lisp
COMMON-LISP:PHASE
  [symbol]

PHASE names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (SINGLE-FLOAT -3.1415927 3.1415927)
                      (DOUBLE-FLOAT -3.141592653589793d0
                       3.141592653589793d0))
                  &OPTIONAL))
  Documentation:
    Return the angle part of the polar representation of a complex number.
      For complex numbers, this is (atan (imagpart number) (realpart number)).
      For non-complex positive numbers, this is 0. For non-complex negative
      numbers this is PI.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `PLUSP`

```lisp
COMMON-LISP:PLUSP
  [symbol]

PLUSP names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (REAL) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Is this real number strictly positive?
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `POSITION`

```lisp
COMMON-LISP:POSITION
  [symbol]

POSITION names a compiled function:
  Lambda-list: (ITEM SEQUENCE &REST ARGS &KEY FROM-END (START 0) END
                KEY TEST TEST-NOT)
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T SEQUENCE &REST T &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))) (:FROM-END T)
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES (OR (MOD 35184372088831) NULL) &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &REST T &KEY (:TEST . #1=((OR FUNCTION SYMBOL)))
                  (:TEST-NOT . #1#) (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:FROM-END T) (:KEY . #1#))
                 (VALUES (OR (MOD 35184372088831) NULL) &OPTIONAL))
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `POSITION-IF`

```lisp
COMMON-LISP:POSITION-IF
  [symbol]

POSITION-IF names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                END KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES (OR (MOD 35184372088831) NULL) &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES (OR (MOD 35184372088831) NULL) &OPTIONAL))
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `POSITION-IF-NOT`

```lisp
COMMON-LISP:POSITION-IF-NOT
  [symbol]

POSITION-IF-NOT names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                END KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES (OR (MOD 35184372088831) NULL) &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES (OR (MOD 35184372088831) NULL) &OPTIONAL))
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `PPRINT`

```lisp
COMMON-LISP:PPRINT
  [symbol]

PPRINT names a compiled function:
  Lambda-list: (OBJECT &OPTIONAL STREAM)
  Declared type: (FUNCTION (T &OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES &OPTIONAL))
  Documentation:
    Prettily output OBJECT preceded by a newline.
  Source file: SYS:SRC;CODE;PRINT.LISP

```

### `PPRINT-DISPATCH`

```lisp
COMMON-LISP:PPRINT-DISPATCH
  [symbol]

PPRINT-DISPATCH names a compiled function:
  Lambda-list: (OBJECT &OPTIONAL (TABLE *PRINT-PPRINT-DISPATCH*))
  Declared type: (FUNCTION
                  (T &OPTIONAL
                   (OR SB-PRETTY:PPRINT-DISPATCH-TABLE NULL))
                  (VALUES (OR FUNCTION SYMBOL) BOOLEAN &OPTIONAL))
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `PPRINT-FILL`

```lisp
COMMON-LISP:PPRINT-FILL
  [symbol]

PPRINT-FILL names a compiled function:
  Lambda-list: (STREAM LIST &OPTIONAL (COLON? T) ATSIGN?)
  Declared type: (FUNCTION ((OR STREAM BOOLEAN) T &OPTIONAL T T)
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Output LIST to STREAM putting :FILL conditional newlines between each
       element. If COLON? is NIL (defaults to T), then no parens are printed
       around the output. ATSIGN? is ignored (but allowed so that PPRINT-FILL
       can be used with the ~/.../ format directive.
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `PPRINT-INDENT`

```lisp
COMMON-LISP:PPRINT-INDENT
  [symbol]

PPRINT-INDENT names a compiled function:
  Lambda-list: (RELATIVE-TO N &OPTIONAL STREAM)
  Declared type: (FUNCTION
                  ((MEMBER :CURRENT :BLOCK) REAL &OPTIONAL
                   (OR STREAM BOOLEAN))
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Specify the indentation to use in the current logical block if
    STREAM (which defaults to *STANDARD-OUTPUT*) is a pretty-printing
    stream and do nothing if not. (See PPRINT-LOGICAL-BLOCK.) N is the
    indentation to use (in ems, the width of an ``m'') and RELATIVE-TO can
    be either:
    
         :BLOCK - Indent relative to the column the current logical block
            started on.
    
         :CURRENT - Indent relative to the current column.
    
    The new indentation value does not take effect until the following
    line break.
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `PPRINT-LINEAR`

```lisp
COMMON-LISP:PPRINT-LINEAR
  [symbol]

PPRINT-LINEAR names a compiled function:
  Lambda-list: (STREAM LIST &OPTIONAL (COLON? T) ATSIGN?)
  Declared type: (FUNCTION ((OR STREAM BOOLEAN) T &OPTIONAL T T)
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Output LIST to STREAM putting :LINEAR conditional newlines between each
       element. If COLON? is NIL (defaults to T), then no parens are printed
       around the output. ATSIGN? is ignored (but allowed so that PPRINT-LINEAR
       can be used with the ~/.../ format directive.
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `PPRINT-NEWLINE`

```lisp
COMMON-LISP:PPRINT-NEWLINE
  [symbol]

PPRINT-NEWLINE names a compiled function:
  Lambda-list: (KIND &OPTIONAL STREAM)
  Declared type: (FUNCTION
                  ((MEMBER :MANDATORY :MISER :FILL :LINEAR) &OPTIONAL
                   (OR STREAM BOOLEAN))
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Output a conditional newline to STREAM (which defaults to
       *STANDARD-OUTPUT*) if it is a pretty-printing stream, and do
       nothing if not. KIND can be one of:
         :LINEAR - A line break is inserted if and only if the immediately
            containing section cannot be printed on one line.
         :MISER - Same as LINEAR, but only if ``miser-style'' is in effect.
            (See *PRINT-MISER-WIDTH*.)
         :FILL - A line break is inserted if and only if either:
           (a) the following section cannot be printed on the end of the
               current line,
           (b) the preceding section was not printed on a single line, or
           (c) the immediately containing section cannot be printed on one
               line and miser-style is in effect.
         :MANDATORY - A line break is always inserted.
       When a line break is inserted by any type of conditional newline, any
       blanks that immediately precede the conditional newline are omitted
       from the output and indentation is introduced at the beginning of the
       next line. (See PPRINT-INDENT.)
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `PPRINT-TAB`

```lisp
COMMON-LISP:PPRINT-TAB
  [symbol]

PPRINT-TAB names a compiled function:
  Lambda-list: (KIND COLNUM COLINC &OPTIONAL STREAM)
  Declared type: (FUNCTION
                  ((MEMBER :SECTION-RELATIVE :LINE-RELATIVE :SECTION
                           :LINE)
                   UNSIGNED-BYTE UNSIGNED-BYTE &OPTIONAL
                   (OR STREAM BOOLEAN))
                  (VALUES NULL &OPTIONAL))
  Documentation:
    If STREAM (which defaults to *STANDARD-OUTPUT*) is a pretty-printing
       stream, perform tabbing based on KIND, otherwise do nothing. KIND can
       be one of:
         :LINE - Tab to column COLNUM. If already past COLNUM tab to the next
           multiple of COLINC.
         :SECTION - Same as :LINE, but count from the start of the current
           section, not the start of the line.
         :LINE-RELATIVE - Output COLNUM spaces, then tab to the next multiple of
           COLINC.
         :SECTION-RELATIVE - Same as :LINE-RELATIVE, but count from the start
           of the current section, not the start of the line.
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `PPRINT-TABULAR`

```lisp
COMMON-LISP:PPRINT-TABULAR
  [symbol]

PPRINT-TABULAR names a compiled function:
  Lambda-list: (STREAM LIST &OPTIONAL (COLON? T) ATSIGN? TABSIZE)
  Declared type: (FUNCTION
                  ((OR STREAM BOOLEAN) T &OPTIONAL T T UNSIGNED-BYTE)
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION
                 ((OR STREAM BOOLEAN) T &OPTIONAL T T
                  (OR UNSIGNED-BYTE NULL))
                 (VALUES NULL &OPTIONAL))
  Documentation:
    Output LIST to STREAM tabbing to the next column that is an even multiple
       of TABSIZE (which defaults to 16) between each element. :FILL style
       conditional newlines are also output between each element. If COLON? is
       NIL (defaults to T), then no parens are printed around the output.
       ATSIGN? is ignored (but allowed so that PPRINT-TABULAR can be used with
       the ~/.../ format directive.
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `PRIN1`

```lisp
COMMON-LISP:PRIN1
  [symbol]

PRIN1 names a compiled function:
  Lambda-list: (OBJECT &OPTIONAL STREAM)
  Declared type: (FUNCTION (T &OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
  Documentation:
    Output a mostly READable printed representation of OBJECT on the specified
      STREAM.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;PRINT.LISP

```

### `PRIN1-TO-STRING`

```lisp
COMMON-LISP:PRIN1-TO-STRING
  [symbol]

PRIN1-TO-STRING names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Return the printed representation of OBJECT as a string with
       slashification on.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;PRINT.LISP

```

### `PRINC`

```lisp
COMMON-LISP:PRINC
  [symbol]

PRINC names a compiled function:
  Lambda-list: (OBJECT &OPTIONAL STREAM)
  Declared type: (FUNCTION (T &OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
  Documentation:
    Output an aesthetic but not necessarily READable printed representation
      of OBJECT on the specified STREAM.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;PRINT.LISP

```

### `PRINC-TO-STRING`

```lisp
COMMON-LISP:PRINC-TO-STRING
  [symbol]

PRINC-TO-STRING names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Return the printed representation of OBJECT as a string with
      slashification off.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;PRINT.LISP

```

### `PRINT`

```lisp
COMMON-LISP:PRINT
  [symbol]

PRINT names a compiled function:
  Lambda-list: (OBJECT &OPTIONAL STREAM)
  Declared type: (FUNCTION (T &OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
  Documentation:
    Output a newline, the mostly READable printed representation of OBJECT, and
      space to the specified STREAM.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;PRINT.LISP

```

### `PRINT-NOT-READABLE-OBJECT`

```lisp
COMMON-LISP:PRINT-NOT-READABLE-OBJECT
  [symbol]

PRINT-NOT-READABLE-OBJECT names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `PROBE-FILE`

```lisp
COMMON-LISP:PROBE-FILE
  [symbol]

PROBE-FILE names a compiled function:
  Lambda-list: (PATHSPEC)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES (OR PATHNAME NULL) &OPTIONAL))
  Documentation:
    Return the truename of PATHSPEC if the truename can be found,
    or NIL otherwise.  See TRUENAME for more information.
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `PROCLAIM`

```lisp
COMMON-LISP:PROCLAIM
  [symbol]

PROCLAIM names a compiled function:
  Lambda-list: (RAW-FORM)
  Declared type: (FUNCTION (LIST) (VALUES &OPTIONAL))
  Known attributes: recursive
  Source file: SYS:SRC;COMPILER;PROCLAIM.LISP

```

### `PROVIDE`

```lisp
COMMON-LISP:PROVIDE
  [symbol]

PROVIDE names a compiled function:
  Lambda-list: (MODULE-NAME)
  Derived type: (FUNCTION (T) (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Adds a new module name to *MODULES* indicating that it has been loaded.
       Module-name is a string designator
  Source file: SYS:SRC;CODE;MODULE.LISP

```

### `RANDOM`

```lisp
COMMON-LISP:RANDOM
  [symbol]

RANDOM names a compiled function:
  Lambda-list: (ARG &OPTIONAL (STATE *RANDOM-STATE*))
  Declared type: (FUNCTION
                  ((OR (FLOAT (0.0)) (INTEGER 1)) &OPTIONAL
                   RANDOM-STATE)
                  (VALUES (OR (FLOAT 0.0) UNSIGNED-BYTE) &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) *)
  Source file: SYS:SRC;CODE;TARGET-RANDOM.LISP

```

### `RANDOM-STATE-P`

```lisp
COMMON-LISP:RANDOM-STATE-P
  [symbol]

RANDOM-STATE-P names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;RANDOM.LISP

```

### `RASSOC`

```lisp
COMMON-LISP:RASSOC
  [symbol]

RASSOC names a compiled function:
  Lambda-list: (ITEM ALIST &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Return the cons in ALIST whose CDR is equal (by a given test or EQL) to
       the ITEM.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `RASSOC-IF`

```lisp
COMMON-LISP:RASSOC-IF
  [symbol]

RASSOC-IF names a compiled function:
  Lambda-list: (PREDICATE ALIST &KEY KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) LIST &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &KEY (:KEY T)) (VALUES LIST &OPTIONAL))
  Documentation:
    Return the first cons in ALIST whose CDR satisfies PREDICATE. If KEY
      is supplied, apply it to the CDR of each cons before testing.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `RASSOC-IF-NOT`

```lisp
COMMON-LISP:RASSOC-IF-NOT
  [symbol]

RASSOC-IF-NOT names a compiled function:
  Lambda-list: (PREDICATE ALIST &KEY KEY)
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) LIST &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T T &KEY (:KEY T)) (VALUES LIST &OPTIONAL))
  Documentation:
    Return the first cons in ALIST whose CDR does not satisfy PREDICATE.
      If KEY is supplied, apply it to the CDR of each cons before testing.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `RATIONAL`

```lisp
COMMON-LISP:RATIONAL
  [symbol]

RATIONAL names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION (REAL) (VALUES RATIONAL &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES RATIONAL &OPTIONAL))
  Documentation:
    RATIONAL produces a rational number for any real numeric argument. This is
      more efficient than RATIONALIZE, but it assumes that floating-point is
      completely accurate, giving a result that isn't as pretty.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

RATIONAL names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:RATIONAL>:
  Class precedence-list: RATIONAL, REAL, NUMBER, T
  Direct superclasses: REAL
  Direct subclasses: INTEGER, RATIO
  Sealed.
  No direct slots.

RATIONAL names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (LOW (QUOTE *)) (HIGH (QUOTE *)))

```

### `RATIONALIZE`

```lisp
COMMON-LISP:RATIONALIZE
  [symbol]

RATIONALIZE names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION (REAL) (VALUES RATIONAL &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES NUMBER &OPTIONAL))
  Documentation:
    Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
      representation exploiting the assumption that floats are only accurate to
      their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
          (= x (float (rationalize x) x))
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;TARGET-FLOAT.LISP

```

### `RATIONALP`

```lisp
COMMON-LISP:RATIONALP
  [symbol]

RATIONALP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a RATIONAL, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `READ`

```lisp
COMMON-LISP:READ
  [symbol]

READ names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T)
                (EOF-VALUE NIL) (RECURSIVE-P NIL))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN) T T T)
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T T T T) (VALUES T &OPTIONAL))
  Documentation:
    Read the next Lisp value from STREAM, and return it.
  Source file: SYS:SRC;CODE;READER.LISP

```

### `READ-BYTE`

```lisp
COMMON-LISP:READ-BYTE
  [symbol]

READ-BYTE names a compiled function:
  Lambda-list: (STREAM &OPTIONAL (EOF-ERROR-P T) EOF-VALUE)
  Declared type: (FUNCTION (STREAM &OPTIONAL T T) (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T T) (VALUES T &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `READ-CHAR`

```lisp
COMMON-LISP:READ-CHAR
  [symbol]

READ-CHAR names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T)
                EOF-VALUE RECURSIVE-P)
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN) T T T)
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T T T T) (VALUES T &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `READ-CHAR-NO-HANG`

```lisp
COMMON-LISP:READ-CHAR-NO-HANG
  [symbol]

READ-CHAR-NO-HANG names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T)
                EOF-VALUE RECURSIVE-P)
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN) T T T)
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T T T T) *)
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `READ-DELIMITED-LIST`

```lisp
COMMON-LISP:READ-DELIMITED-LIST
  [symbol]

READ-DELIMITED-LIST names a compiled function:
  Lambda-list: (ENDCHAR &OPTIONAL (INPUT-STREAM *STANDARD-INPUT*)
                RECURSIVE-P)
  Declared type: (FUNCTION (CHARACTER &OPTIONAL (OR STREAM BOOLEAN) T)
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T T) (VALUES T &OPTIONAL))
  Documentation:
    Read Lisp values from INPUT-STREAM until the next character after a
       value's representation is ENDCHAR, and return the objects as a list.
  Source file: SYS:SRC;CODE;READER.LISP

```

### `READ-FROM-STRING`

```lisp
COMMON-LISP:READ-FROM-STRING
  [symbol]

READ-FROM-STRING names a compiled function:
  Lambda-list: (STRING &OPTIONAL (EOF-ERROR-P T) EOF-VALUE &KEY
                       (START 0) END PRESERVE-WHITESPACE)
  Declared type: (FUNCTION
                  (STRING &OPTIONAL T T &KEY
                          (:START (UNSIGNED-BYTE 45))
                          (:END (OR NULL (UNSIGNED-BYTE 45)))
                          (:PRESERVE-WHITESPACE T))
                  (VALUES T (UNSIGNED-BYTE 45) &OPTIONAL))
  Documentation:
    The characters of string are successively given to the lisp reader
       and the lisp object built by the reader is returned. Macro chars
       will take effect.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;READER.LISP

READ-FROM-STRING has a compiler-macro:
  Source file: SYS:SRC;CODE;CMACROS.LISP

```

### `READ-LINE`

```lisp
COMMON-LISP:READ-LINE
  [symbol]

READ-LINE names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T)
                EOF-VALUE RECURSIVE-P)
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN) T T T)
                  (VALUES T BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T T T T) *)
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `READ-PRESERVING-WHITESPACE`

```lisp
COMMON-LISP:READ-PRESERVING-WHITESPACE
  [symbol]

READ-PRESERVING-WHITESPACE names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-INPUT*) (EOF-ERROR-P T)
                (EOF-VALUE NIL) (RECURSIVE-P NIL))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN) T T T)
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T T T T) (VALUES T &OPTIONAL))
  Documentation:
    Read from STREAM and return the value read, preserving any whitespace
       that followed the object.
  Source file: SYS:SRC;CODE;READER.LISP

```

### `READ-SEQUENCE`

```lisp
COMMON-LISP:READ-SEQUENCE
  [symbol]

READ-SEQUENCE names a compiled function:
  Lambda-list: (SEQ STREAM &KEY (START 0) END)
  Declared type: (FUNCTION
                  (SEQUENCE STREAM &KEY (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION (T T &KEY (:START . #1=(T)) (:END . #1#)) *)
  Documentation:
    Destructively modify SEQ by reading elements from STREAM.
      That part of SEQ bounded by START and END is destructively modified by
      copying successive elements into it from STREAM. If the end of file
      for STREAM is reached before copying all elements of the subsequence,
      then the extra elements near the end of sequence are not updated, and
      the index of the next element is returned.
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `READTABLE-CASE`

```lisp
COMMON-LISP:READTABLE-CASE
  [symbol]

READTABLE-CASE names a compiled function:
  Lambda-list: (READTABLE)
  Derived type: (FUNCTION (T)
                 (VALUES (MEMBER :UPCASE :DOWNCASE :PRESERVE :INVERT)
                         &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;READER.LISP

(SETF READTABLE-CASE) names a compiled function:
  Lambda-list: (CASE READTABLE)
  Derived type: (FUNCTION (T T)
                 (VALUES (MEMBER :UPCASE :DOWNCASE :PRESERVE :INVERT)
                         &OPTIONAL))
  Source file: SYS:SRC;CODE;READER.LISP

```

### `(SETF READTABLE-CASE)`

```lisp
COMMON-LISP-USER::|(SETF READTABLE-CASE)|
  [symbol]

```

### `READTABLEP`

```lisp
COMMON-LISP:READTABLEP
  [symbol]

READTABLEP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;READTABLE.LISP

```

### `REALP`

```lisp
COMMON-LISP:REALP
  [symbol]

REALP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a REAL, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `REALPART`

```lisp
COMMON-LISP:REALPART
  [symbol]

REALPART names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES REAL &OPTIONAL))
  Documentation:
    Extract the real part of a number.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `REDUCE`

```lisp
COMMON-LISP:REDUCE
  [symbol]

REDUCE names a compiled function:
  Lambda-list: (FUNCTION SEQUENCE &REST ARGS &KEY (KEY NIL) FROM-END
                (START 0) (END NIL) (INITIAL-VALUE NIL IVP))
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:INITIAL-VALUE T) (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY
                  (:FROM-END . #2=(T))
                  (:START . #3=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #3#)) (:INITIAL-VALUE . #2#)
                  (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `REM`

```lisp
COMMON-LISP:REM
  [symbol]

REM names a compiled function:
  Lambda-list: (NUMBER DIVISOR)
  Declared type: (FUNCTION (REAL REAL) (VALUES REAL &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES REAL &OPTIONAL))
  Documentation:
    Return second result of TRUNCATE.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `REMHASH`

```lisp
COMMON-LISP:REMHASH
  [symbol]

REMHASH names a compiled function:
  Lambda-list: (KEY HASH-TABLE)
  Declared type: (FUNCTION (T HASH-TABLE) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Remove the entry in HASH-TABLE associated with KEY. Return T if
    there was such an entry, or NIL if not.
  Source file: SYS:SRC;CODE;TARGET-HASH-TABLE.LISP

```

### `REMOVE`

```lisp
COMMON-LISP:REMOVE
  [symbol]

REMOVE names a compiled function:
  Lambda-list: (ITEM SEQUENCE &REST ARGS &KEY FROM-END
                (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START 0)
                (END NIL) (COUNT NIL) (KEY NIL))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT :KEY)
  Declared type: (FUNCTION
                  (T SEQUENCE &REST T &KEY (:FROM-END T)
                   (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:COUNT (OR NULL INTEGER))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &REST T &KEY (:FROM-END T)
                  (:TEST . #1=((OR FUNCTION SYMBOL))) (:TEST-NOT . #1#)
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:COUNT (OR NULL INTEGER))
                  (:KEY . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a copy of SEQUENCE with elements satisfying the test (default is
       EQL) with ITEM removed.
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `REMOVE-DUPLICATES`

```lisp
COMMON-LISP:REMOVE-DUPLICATES
  [symbol]

REMOVE-DUPLICATES names a compiled function:
  Lambda-list: (SEQUENCE &REST ARGS &KEY (TEST (FUNCTION EQL))
                (TEST-NOT NIL) (START 0) (END NIL) FROM-END (KEY NIL))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT :KEY)
  Declared type: (FUNCTION
                  (SEQUENCE &REST T &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))) (:FROM-END T)
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (T &REST T &KEY (:TEST . #1=((OR FUNCTION SYMBOL)))
                  (:TEST-NOT . #1#) (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:FROM-END T) (:KEY . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    The elements of SEQUENCE are compared pairwise, and if any two match,
       the one occurring earlier is discarded, unless FROM-END is true, in
       which case the one later in the sequence is discarded. The resulting
       sequence is returned.
    
       The :TEST-NOT argument is deprecated.
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `REMOVE-IF`

```lisp
COMMON-LISP:REMOVE-IF
  [symbol]

REMOVE-IF names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                (END NIL) (COUNT NIL) (KEY NIL))
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:COUNT (OR NULL INTEGER))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:COUNT (OR NULL INTEGER))
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a copy of sequence with elements satisfying PREDICATE removed.
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `REMOVE-IF-NOT`

```lisp
COMMON-LISP:REMOVE-IF-NOT
  [symbol]

REMOVE-IF-NOT names a compiled function:
  Lambda-list: (PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0)
                (END NIL) (COUNT NIL) (KEY NIL))
  Dynamic-extent arguments: positional=(0), keyword=(:KEY)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:COUNT (OR NULL INTEGER))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (#1=(OR FUNCTION SYMBOL) T &REST T &KEY (:FROM-END T)
                  (:COUNT (OR NULL INTEGER))
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a copy of sequence with elements not satisfying PREDICATE removed.
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `REMPROP`

```lisp
COMMON-LISP:REMPROP
  [symbol]

REMPROP names a compiled function:
  Lambda-list: (SYMBOL INDICATOR)
  Declared type: (FUNCTION (SYMBOL T) (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (SYMBOL T) (VALUES LIST &OPTIONAL))
  Documentation:
    Look on property list of SYMBOL for property with specified
      INDICATOR. If found, splice this indicator and its value out of
      the plist, and return the tail of the original list starting with
      INDICATOR. If not found, return () with no side effects.
    
      NOTE: The ANSI specification requires REMPROP to return true (not false)
      or false (the symbol NIL). Portable code should not rely on any other value.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `RENAME-FILE`

```lisp
COMMON-LISP:RENAME-FILE
  [symbol]

RENAME-FILE names a compiled function:
  Lambda-list: (FILE NEW-NAME)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   (OR STRING PATHNAME))
                  (VALUES PATHNAME PATHNAME PATHNAME &OPTIONAL))
  Documentation:
    Rename FILE to have the specified NEW-NAME. If FILE is a stream open to a
    file, then the associated file is renamed.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `RENAME-PACKAGE`

```lisp
COMMON-LISP:RENAME-PACKAGE
  [symbol]

RENAME-PACKAGE names a compiled function:
  Lambda-list: (PACKAGE-DESIGNATOR NAME &OPTIONAL (NICKNAMES NIL))
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER PACKAGE)
                   (OR STRING SYMBOL CHARACTER PACKAGE) &OPTIONAL LIST)
                  (VALUES PACKAGE &OPTIONAL))
  Documentation:
    Changes the name and nicknames for a package.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;DEFPACKAGE.LISP

```

### `REPLACE`

```lisp
COMMON-LISP:REPLACE
  [symbol]

REPLACE names a compiled function:
  Lambda-list: (TARGET-SEQUENCE1 SOURCE-SEQUENCE2 &REST ARGS &KEY
                (START1 0) (END1 NIL) (START2 0) (END2 NIL))
  Declared type: (FUNCTION
                  (SEQUENCE SEQUENCE &REST T &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &REST T &KEY (:START1 . #1=((UNSIGNED-BYTE 45)))
                  (:END1 . #2=((OR NULL . #1#))) (:START2 . #1#)
                  (:END2 . #2#))
                 (VALUES SEQUENCE &OPTIONAL))
  Documentation:
    Destructively modifies SEQUENCE1 by copying successive elements
    into it from the SEQUENCE2.
    
    Elements are copied to the subsequence bounded by START1 and END1,
    from the subsequence bounded by START2 and END2. If these subsequences
    are not of the same length, then the shorter length determines how
    many elements are copied.
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `REQUIRE`

```lisp
COMMON-LISP:REQUIRE
  [symbol]

REQUIRE names a compiled function:
  Lambda-list: (MODULE-NAME &OPTIONAL PATHNAMES)
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES LIST &OPTIONAL))
  Documentation:
    Loads a module, unless it already has been loaded. PATHNAMES, if supplied,
       is a designator for a list of pathnames to be loaded if the module
       needs to be. If PATHNAMES is not supplied, functions from the list
       *MODULE-PROVIDER-FUNCTIONS* are called in order with MODULE-NAME
       as an argument, until one of them returns non-NIL.  User code is
       responsible for calling PROVIDE to indicate a successful load of the
       module.
  Source file: SYS:SRC;CODE;MODULE.LISP

```

### `REST`

```lisp
COMMON-LISP:REST
  [symbol]

REST names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Means the same as the cdr of a list.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF REST) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF REST) has setf-expansion: SB-KERNEL:%RPLACD

```

### `(SETF REST)`

```lisp
COMMON-LISP-USER::|(SETF REST)|
  [symbol]

```

### `RESTART-NAME`

```lisp
COMMON-LISP:RESTART-NAME
  [symbol]

RESTART-NAME names a compiled function:
  Lambda-list: (INSTANCE)
  Declared type: (FUNCTION (RESTART) (VALUES SYMBOL &OPTIONAL))
  Documentation:
    Return the name of the given restart object.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;RESTART.LISP

```

### `REVAPPEND`

```lisp
COMMON-LISP:REVAPPEND
  [symbol]

REVAPPEND names a compiled function:
  Lambda-list: (X Y)
  Declared type: (FUNCTION (LIST T) (VALUES T &OPTIONAL))
  Documentation:
    Return (append (reverse x) y).
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `REVERSE`

```lisp
COMMON-LISP:REVERSE
  [symbol]

REVERSE names a compiled function:
  Lambda-list: (SEQUENCE)
  Declared type: (FUNCTION (SEQUENCE)
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return a new sequence containing the same elements but in reverse order.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `ROOM`

```lisp
COMMON-LISP:ROOM
  [symbol]

ROOM names a compiled function:
  Lambda-list: (&OPTIONAL (VERBOSITY DEFAULT))
  Declared type: (FUNCTION (&OPTIONAL (MEMBER :DEFAULT NIL T))
                  (VALUES &OPTIONAL))
  Documentation:
    Print to *STANDARD-OUTPUT* information about the state of internal
      storage and its management. The optional argument controls the
      verbosity of output. If it is T, ROOM prints out a maximal amount of
      information. If it is NIL, ROOM prints out a minimal amount of
      information. If it is :DEFAULT or it is not supplied, ROOM prints out
      an intermediate amount of information.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;ROOM.LISP

```

### `ROUND`

```lisp
COMMON-LISP:ROUND
  [symbol]

ROUND names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES INTEGER REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES INTEGER NUMBER &OPTIONAL))
  Documentation:
    Rounds number (or number/divisor) to nearest integer.
      The second returned value is the remainder.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `ROW-MAJOR-AREF`

```lisp
COMMON-LISP:ROW-MAJOR-AREF
  [symbol]

ROW-MAJOR-AREF names a compiled function:
  Lambda-list: (ARRAY INDEX)
  Declared type: (FUNCTION (ARRAY (UNSIGNED-BYTE 45))
                  (VALUES T &OPTIONAL))
  Documentation:
    Return the element of array corresponding to the row-major index. This is
       SETFable.
  Known attributes: foldable
  Source file: SYS:SRC;CODE;ARRAY.LISP

(SETF ROW-MAJOR-AREF) names a compiled function:
  Lambda-list: (NEWVAL ARRAY INDEX)
  Derived type: (FUNCTION (T ARRAY (UNSIGNED-BYTE 45))
                 (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF ROW-MAJOR-AREF) has setf-expansion: SB-KERNEL:%SET-ROW-MAJOR-AREF

```

### `(SETF ROW-MAJOR-AREF)`

```lisp
COMMON-LISP-USER::|(SETF ROW-MAJOR-AREF)|
  [symbol]

```

### `RPLACA`

```lisp
COMMON-LISP:RPLACA
  [symbol]

RPLACA names a compiled function:
  Lambda-list: (CONS X)
  Declared type: (FUNCTION (CONS T) (VALUES CONS &OPTIONAL))
  Documentation:
    Change the CAR of CONS to X and return the CONS.
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `RPLACD`

```lisp
COMMON-LISP:RPLACD
  [symbol]

RPLACD names a compiled function:
  Lambda-list: (CONS X)
  Declared type: (FUNCTION (CONS T) (VALUES CONS &OPTIONAL))
  Documentation:
    Change the CDR of CONS to X and return the CONS.
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `SBIT`

```lisp
COMMON-LISP:SBIT
  [symbol]

SBIT names a compiled function:
  Lambda-list: (SIMPLE-BIT-ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION
                  ((SIMPLE-ARRAY BIT) &REST (UNSIGNED-BYTE 45))
                  (VALUES BIT &OPTIONAL))
  Derived type: (FUNCTION ((SIMPLE-ARRAY BIT) &REST T)
                 (VALUES BIT &OPTIONAL))
  Documentation:
    Return the bit from SIMPLE-BIT-ARRAY at the specified SUBSCRIPTS.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

(SETF SBIT) names a compiled function:
  Lambda-list: (NEW-VALUE BIT-ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION
                  (BIT (SIMPLE-ARRAY BIT) &REST (UNSIGNED-BYTE 45))
                  (VALUES BIT &OPTIONAL))
  Derived type: (FUNCTION (BIT (SIMPLE-ARRAY BIT) &REST T)
                 (VALUES BIT &OPTIONAL))
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `(SETF SBIT)`

```lisp
COMMON-LISP-USER::|(SETF SBIT)|
  [symbol]

```

### `SCALE-FLOAT`

```lisp
COMMON-LISP:SCALE-FLOAT
  [symbol]

SCALE-FLOAT names a compiled function:
  Lambda-list: (F EX)
  Declared type: (FUNCTION (FLOAT INTEGER) (VALUES FLOAT &OPTIONAL))
  Derived type: (FUNCTION (T INTEGER) (VALUES FLOAT &OPTIONAL))
  Documentation:
    Return the value (* f (expt (float 2 f) ex)), but with no unnecessary loss
      of precision or overflow.
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

```

### `SCHAR`

```lisp
COMMON-LISP:SCHAR
  [symbol]

SCHAR names a compiled function:
  Lambda-list: (STRING INDEX)
  Declared type: (FUNCTION (SIMPLE-STRING (UNSIGNED-BYTE 45))
                  (VALUES CHARACTER &OPTIONAL))
  Documentation:
    SCHAR returns the character object at an indexed position in a string
       just as CHAR does, except the string must be a simple-string.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

(SETF SCHAR) names a compiled function:
  Lambda-list: (NEWVAL STRING INDEX)
  Derived type: (FUNCTION (CHARACTER SIMPLE-STRING (UNSIGNED-BYTE 45))
                 (VALUES CHARACTER &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF SCHAR) has setf-expansion: SB-KERNEL:%SCHARSET

```

### `(SETF SCHAR)`

```lisp
COMMON-LISP-USER::|(SETF SCHAR)|
  [symbol]

```

### `SEARCH`

```lisp
COMMON-LISP:SEARCH
  [symbol]

SEARCH names a compiled function:
  Lambda-list: (SUB-SEQUENCE1 MAIN-SEQUENCE2 &REST ARGS &KEY FROM-END
                (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START1 0)
                (END1 NIL) (START2 0) (END2 NIL) (KEY NIL))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT :KEY)
  Declared type: (FUNCTION
                  (SEQUENCE SEQUENCE &REST T &KEY (:FROM-END T)
                   (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Derived type: (FUNCTION
                 (SEQUENCE T &REST T &KEY (:FROM-END T)
                  (:TEST . #1=((OR FUNCTION SYMBOL))) (:TEST-NOT . #1#)
                  (:START1 . #2=((UNSIGNED-BYTE 45)))
                  (:END1 . #3=(#4=(OR NULL . #2#))) (:START2 . #2#)
                  (:END2 . #3#) (:KEY . #1#))
                 (VALUES #4# &OPTIONAL))
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `SECOND`

```lisp
COMMON-LISP:SECOND
  [symbol]

SECOND names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 2nd object in a list or NIL if there is no 2nd object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF SECOND) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF SECOND) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF SECOND)`

```lisp
COMMON-LISP-USER::|(SETF SECOND)|
  [symbol]

```

### `SET`

```lisp
COMMON-LISP:SET
  [symbol]

SET names a compiled function:
  Lambda-list: (SYMBOL NEW-VALUE)
  Declared type: (FUNCTION (SYMBOL T) (VALUES T &OPTIONAL))
  Documentation:
    Set SYMBOL's value cell to NEW-VALUE.
  Source file: SYS:SRC;CODE;SYMBOL.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION SET(..

```

### `SET-DIFFERENCE`

```lisp
COMMON-LISP:SET-DIFFERENCE
  [symbol]

SET-DIFFERENCE names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return the elements of LIST1 which are not in LIST2.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `SET-DISPATCH-MACRO-CHARACTER`

```lisp
COMMON-LISP:SET-DISPATCH-MACRO-CHARACTER
  [symbol]

SET-DISPATCH-MACRO-CHARACTER names a compiled function:
  Lambda-list: (DISP-CHAR SUB-CHAR FUNCTION &OPTIONAL
                (RT-DESIGNATOR *READTABLE*))
  Declared type: (FUNCTION
                  (CHARACTER CHARACTER (OR FUNCTION SYMBOL) &OPTIONAL
                             (OR READTABLE NULL))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Cause FUNCTION to be called whenever the reader reads DISP-CHAR
       followed by SUB-CHAR.
  Known attributes: call
  Source file: SYS:SRC;CODE;READER.LISP

```

### `SET-EXCLUSIVE-OR`

```lisp
COMMON-LISP:SET-EXCLUSIVE-OR
  [symbol]

SET-EXCLUSIVE-OR names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    Return new list of elements appearing exactly once in LIST1 and LIST2.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `SET-MACRO-CHARACTER`

```lisp
COMMON-LISP:SET-MACRO-CHARACTER
  [symbol]

SET-MACRO-CHARACTER names a compiled function:
  Lambda-list: (CHAR FUNCTION &OPTIONAL (NON-TERMINATINGP NIL)
                     (RT-DESIGNATOR *READTABLE*))
  Declared type: (FUNCTION
                  (CHARACTER (OR FUNCTION SYMBOL) &OPTIONAL T
                             (OR READTABLE NULL))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Causes CHAR to be a macro character which invokes FUNCTION when seen
       by the reader. The NON-TERMINATINGP flag can be used to make the macro
       character non-terminating, i.e. embeddable in a symbol name.
  Known attributes: call
  Source file: SYS:SRC;CODE;READER.LISP

```

### `SET-PPRINT-DISPATCH`

```lisp
COMMON-LISP:SET-PPRINT-DISPATCH
  [symbol]

SET-PPRINT-DISPATCH names a compiled function:
  Lambda-list: (TYPE FUNCTION &OPTIONAL (PRIORITY 0)
                (TABLE *PRINT-PPRINT-DISPATCH*))
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS)
                   (OR FUNCTION SYMBOL) &OPTIONAL REAL
                   SB-PRETTY:PPRINT-DISPATCH-TABLE)
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION
                 (T (OR FUNCTION SYMBOL) &OPTIONAL REAL
                  SB-PRETTY:PPRINT-DISPATCH-TABLE)
                 (VALUES NULL &OPTIONAL))
  Known attributes: call
  Source file: SYS:SRC;CODE;PPRINT.LISP

```

### `SET-SYNTAX-FROM-CHAR`

```lisp
COMMON-LISP:SET-SYNTAX-FROM-CHAR
  [symbol]

SET-SYNTAX-FROM-CHAR names a compiled function:
  Lambda-list: (TO-CHAR FROM-CHAR &OPTIONAL (TO-READTABLE *READTABLE*)
                (FROM-READTABLE NIL))
  Declared type: (FUNCTION
                  (CHARACTER CHARACTER &OPTIONAL READTABLE
                             (OR READTABLE NULL))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Causes the syntax of TO-CHAR to be the same as FROM-CHAR in the optional
    readtable (defaults to the current readtable). The FROM-TABLE defaults to the
    standard Lisp readtable when NIL.
  Source file: SYS:SRC;CODE;READER.LISP

```

### `SEVENTH`

```lisp
COMMON-LISP:SEVENTH
  [symbol]

SEVENTH names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 7th object in a list or NIL if there is no 7th object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF SEVENTH) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF SEVENTH) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF SEVENTH)`

```lisp
COMMON-LISP-USER::|(SETF SEVENTH)|
  [symbol]

```

### `SHADOW`

```lisp
COMMON-LISP:SHADOW
  [symbol]

SHADOW names a compiled function:
  Lambda-list: (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  ((OR STRING CHARACTER CONS SYMBOL) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Make an internal symbol in PACKAGE with the same name as each of the
    specified SYMBOLS. If a symbol with the given name is already present in
    PACKAGE, then the existing symbol is placed in the shadowing symbols list if
    it is not already present.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `SHADOWING-IMPORT`

```lisp
COMMON-LISP:SHADOWING-IMPORT
  [symbol]

SHADOWING-IMPORT names a compiled function:
  Lambda-list: (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Import SYMBOLS into package, disregarding any name conflict. If
      a symbol of the same name is present, then it is uninterned.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `SHORT-SITE-NAME`

```lisp
COMMON-LISP:SHORT-SITE-NAME
  [symbol]

SHORT-SITE-NAME names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Documentation:
    Return a string with the abbreviated site name, or NIL if not known.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-MISC.LISP

```

### `SIGNAL`

```lisp
COMMON-LISP:SIGNAL
  [symbol]

SIGNAL names a compiled function:
  Lambda-list: (DATUM &REST ARGUMENTS)
  Declared type: (FUNCTION
                  ((OR STRING FUNCTION SYMBOL CONDITION
                       SB-PCL::CONDITION-CLASS)
                   &REST T)
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) *)
  Documentation:
    Invokes the signal facility on a condition formed from DATUM and
       ARGUMENTS. If the condition is not handled, NIL is returned. If
       (TYPEP condition *BREAK-ON-SIGNALS*) is true, the debugger is invoked
       before any signalling is done.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;COLD-ERROR.LISP

```

### `SIGNUM`

```lisp
COMMON-LISP:SIGNUM
  [symbol]

SIGNUM names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (FLOAT 1.0 1.0) (SINGLE-FLOAT 0.0 0.0)
                      (DOUBLE-FLOAT 0.0d0 0.0d0)
                      (COMPLEX (INTEGER 0 0))
                      (DOUBLE-FLOAT -1.0d0 -1.0d0)
                      (SINGLE-FLOAT -1.0 -1.0) (INTEGER -1 1)
                      (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    If NUMBER is zero, return NUMBER, else return (/ NUMBER (ABS NUMBER)).
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `SIMPLE-BIT-VECTOR-P`

```lisp
COMMON-LISP:SIMPLE-BIT-VECTOR-P
  [symbol]

SIMPLE-BIT-VECTOR-P names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a SIMPLE-BIT-VECTOR, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `SIMPLE-CONDITION-FORMAT-ARGUMENTS`

```lisp
COMMON-LISP:SIMPLE-CONDITION-FORMAT-ARGUMENTS
  [symbol]

SIMPLE-CONDITION-FORMAT-ARGUMENTS names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `SIMPLE-CONDITION-FORMAT-CONTROL`

```lisp
COMMON-LISP:SIMPLE-CONDITION-FORMAT-CONTROL
  [symbol]

SIMPLE-CONDITION-FORMAT-CONTROL names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `SIMPLE-STRING-P`

```lisp
COMMON-LISP:SIMPLE-STRING-P
  [symbol]

SIMPLE-STRING-P names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a SIMPLE-STRING, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `SIMPLE-VECTOR-P`

```lisp
COMMON-LISP:SIMPLE-VECTOR-P
  [symbol]

SIMPLE-VECTOR-P names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a SIMPLE-VECTOR, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `SIN`

```lisp
COMMON-LISP:SIN
  [symbol]

SIN names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR (FLOAT -1.0 1.0) (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (FLOAT -1.0 1.0) (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    Return the sine of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `SINH`

```lisp
COMMON-LISP:SINH
  [symbol]

SINH names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR FLOAT (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    Return the hyperbolic sine of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `SIXTH`

```lisp
COMMON-LISP:SIXTH
  [symbol]

SIXTH names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 6th object in a list or NIL if there is no 6th object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF SIXTH) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF SIXTH) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF SIXTH)`

```lisp
COMMON-LISP-USER::|(SETF SIXTH)|
  [symbol]

```

### `SLEEP`

```lisp
COMMON-LISP:SLEEP
  [symbol]

SLEEP names a compiled function:
  Lambda-list: (SECONDS)
  Declared type: (FUNCTION ((REAL 0)) (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES NULL &OPTIONAL))
  Documentation:
    This function causes execution to be suspended for SECONDS. SECONDS may be
    any non-negative real number.
  Source file: SYS:SRC;CODE;TOPLEVEL.LISP

```

### `SLOT-BOUNDP`

```lisp
COMMON-LISP:SLOT-BOUNDP
  [symbol]

SLOT-BOUNDP names a compiled function:
  Lambda-list: (OBJECT SLOT-NAME)
  Declared type: (FUNCTION (T SYMBOL) (VALUES BOOLEAN &OPTIONAL))
  Known attributes: unwind, any
  Source file: SYS:SRC;PCL;SLOTS.LISP

```

### `SLOT-EXISTS-P`

```lisp
COMMON-LISP:SLOT-EXISTS-P
  [symbol]

SLOT-EXISTS-P names a compiled function:
  Lambda-list: (OBJECT SLOT-NAME)
  Declared type: (FUNCTION (T SYMBOL) (VALUES BOOLEAN &OPTIONAL))
  Known attributes: unwind, any
  Source file: SYS:SRC;PCL;SLOTS.LISP

```

### `SLOT-MAKUNBOUND`

```lisp
COMMON-LISP:SLOT-MAKUNBOUND
  [symbol]

SLOT-MAKUNBOUND names a compiled function:
  Lambda-list: (OBJECT SLOT-NAME)
  Declared type: (FUNCTION (T SYMBOL) (VALUES T &OPTIONAL))
  Known attributes: unwind, any
  Source file: SYS:SRC;PCL;SLOTS.LISP

```

### `SLOT-VALUE`

```lisp
COMMON-LISP:SLOT-VALUE
  [symbol]

SLOT-VALUE names a compiled function:
  Lambda-list: (OBJECT SLOT-NAME)
  Declared type: (FUNCTION (T SYMBOL) (VALUES T &OPTIONAL))
  Known attributes: unwind, any
  Source file: SYS:SRC;PCL;SLOTS.LISP

(SETF SLOT-VALUE) names a compiled function:
  Lambda-list: (NEWVAL OBJECT SLOT-NAME)
  Derived type: (FUNCTION (T T SYMBOL) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF SLOT-VALUE) has setf-expansion: SB-PCL::SET-SLOT-VALUE

```

### `(SETF SLOT-VALUE)`

```lisp
COMMON-LISP-USER::|(SETF SLOT-VALUE)|
  [symbol]

```

### `SOFTWARE-TYPE`

```lisp
COMMON-LISP:SOFTWARE-TYPE
  [symbol]

SOFTWARE-TYPE names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Derived type: (FUNCTION NIL (VALUES (SIMPLE-BASE-STRING 5) &OPTIONAL))
  Documentation:
    Return a string describing the supporting software.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;WIN32-OS.LISP

```

### `SOFTWARE-VERSION`

```lisp
COMMON-LISP:SOFTWARE-VERSION
  [symbol]

SOFTWARE-VERSION names a compiled function:
  Lambda-list: ()
  Declared type: (FUNCTION NIL
                  (VALUES (OR SIMPLE-STRING NULL) &OPTIONAL))
  Derived type: (FUNCTION NIL (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Return a string describing version of the supporting software, or NIL
      if not available.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;WIN32-OS.LISP

```

### `SOME`

```lisp
COMMON-LISP:SOME
  [symbol]

SOME names a compiled function:
  Lambda-list: (PRED FIRST-SEQ &REST MORE-SEQS)
  Dynamic-extent arguments: positional=(0)
  Declared type: (FUNCTION
                  ((OR FUNCTION SYMBOL) SEQUENCE &REST SEQUENCE)
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION ((OR FUNCTION SYMBOL) SEQUENCE &REST T)
                 (VALUES T &OPTIONAL))
  Documentation:
    Apply PREDICATE to the 0-indexed elements of the sequences, then
       possibly to those with index 1, and so on. Return the first
       non-NIL value encountered, or NIL if the end of any sequence is reached.
  Known attributes: call, foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;QUANTIFIERS.LISP

```

### `SORT`

```lisp
COMMON-LISP:SORT
  [symbol]

SORT names a compiled function:
  Lambda-list: (SEQUENCE PREDICATE &REST ARGS &KEY KEY)
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (SEQUENCE (OR FUNCTION SYMBOL) &REST T &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Documentation:
    Destructively sort SEQUENCE. PREDICATE should return non-NIL if
       ARG1 is to precede ARG2.
  Known attributes: call
  Source file: SYS:SRC;CODE;SORT.LISP

```

### `SPECIAL-OPERATOR-P`

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

### `SQRT`

```lisp
COMMON-LISP:SQRT
  [symbol]

SQRT names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the square root of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `STABLE-SORT`

```lisp
COMMON-LISP:STABLE-SORT
  [symbol]

STABLE-SORT names a compiled function:
  Lambda-list: (SEQUENCE PREDICATE &REST ARGS &KEY KEY)
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (SEQUENCE (OR FUNCTION SYMBOL) &REST T &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES SEQUENCE &OPTIONAL))
  Documentation:
    Destructively sort SEQUENCE. PREDICATE should return non-NIL if
       ARG1 is to precede ARG2.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call
  Source file: SYS:SRC;CODE;SORT.LISP

```

### `STANDARD-CHAR-P`

```lisp
COMMON-LISP:STANDARD-CHAR-P
  [symbol]

STANDARD-CHAR-P names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    The argument must be a character object. STANDARD-CHAR-P returns T if the
    argument is a standard character -- one of the 95 ASCII printing characters or
    <return>.
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `STORE-VALUE`

```lisp
COMMON-LISP:STORE-VALUE
  [symbol]

STORE-VALUE names a compiled function:
  Lambda-list: (VALUE &OPTIONAL CONDITION)
  Declared type: (FUNCTION (T &OPTIONAL (OR CONDITION NULL))
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Transfer control and VALUE to a restart named STORE-VALUE, or
    return NIL if none exists.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `STREAM-ERROR-STREAM`

```lisp
COMMON-LISP:STREAM-ERROR-STREAM
  [symbol]

STREAM-ERROR-STREAM names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `STREAM-EXTERNAL-FORMAT`

```lisp
COMMON-LISP:STREAM-EXTERNAL-FORMAT
  [symbol]

STREAM-EXTERNAL-FORMAT names a compiled function:
  Lambda-list: (STREAM)
  Declared type: (FUNCTION (STREAM) (VALUES T &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `STREAMP`

```lisp
COMMON-LISP:STREAMP
  [symbol]

STREAMP names a compiled function:
  Lambda-list: (STREAM)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `STRING`

```lisp
COMMON-LISP:STRING
  [symbol]

STRING names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER))
                  (VALUES STRING &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES STRING &OPTIONAL))
  Documentation:
    Coerces X into a string. If X is a string, X is returned. If X is a
       symbol, its name is returned. If X is a character then a one element
       string containing that character is returned. If X cannot be coerced
       into a string, an error occurs.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

STRING names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:STRING>:
  Class precedence-list: STRING, VECTOR, ARRAY, SEQUENCE, T
  Direct superclasses: VECTOR
  Direct subclasses: BASE-STRING, SB-KERNEL::CHARACTER-STRING,
                     SIMPLE-STRING
  Sealed.
  No direct slots.

STRING names a primitive type-specifier:
  Lambda-list: (&OPTIONAL SIZE)

```

### `STRING-CAPITALIZE`

```lisp
COMMON-LISP:STRING-CAPITALIZE
  [symbol]

STRING-CAPITALIZE names a compiled function:
  Lambda-list: (STRING &KEY (START 0) END)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER) &KEY
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES SIMPLE-STRING &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-DOWNCASE`

```lisp
COMMON-LISP:STRING-DOWNCASE
  [symbol]

STRING-DOWNCASE names a compiled function:
  Lambda-list: (STRING &KEY (START 0) END)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER) &KEY
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES SIMPLE-STRING &OPTIONAL))
  Derived type: (FUNCTION (T &KEY (:START . #1=(T)) (:END . #1#))
                 (VALUES SIMPLE-STRING &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-EQUAL`

```lisp
COMMON-LISP:STRING-EQUAL
  [symbol]

STRING-EQUAL names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Given two strings (string1 and string2), and optional integers start1,
      start2, end1 and end2, compares characters in string1 to characters in
      string2 (using char-equal).
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-GREATERP`

```lisp
COMMON-LISP:STRING-GREATERP
  [symbol]

STRING-GREATERP names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is lexicographically greater than
      the second string, returns the longest common prefix (using char-equal)
      of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-LEFT-TRIM`

```lisp
COMMON-LISP:STRING-LEFT-TRIM
  [symbol]

STRING-LEFT-TRIM names a compiled function:
  Lambda-list: (CHAR-BAG STRING)
  Declared type: (FUNCTION (SEQUENCE (OR STRING SYMBOL CHARACTER))
                  (VALUES STRING &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-LESSP`

```lisp
COMMON-LISP:STRING-LESSP
  [symbol]

STRING-LESSP names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is lexicographically less than
      the second string, returns the longest common prefix (using char-equal)
      of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-NOT-EQUAL`

```lisp
COMMON-LISP:STRING-NOT-EQUAL
  [symbol]

STRING-NOT-EQUAL names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is not lexicographically equal
      to the second string, returns the longest common prefix (using char-equal)
      of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-NOT-GREATERP`

```lisp
COMMON-LISP:STRING-NOT-GREATERP
  [symbol]

STRING-NOT-GREATERP names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is lexicographically less than
      or equal to the second string, returns the longest common prefix
      (using char-equal) of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-NOT-LESSP`

```lisp
COMMON-LISP:STRING-NOT-LESSP
  [symbol]

STRING-NOT-LESSP names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is lexicographically greater
      than or equal to the second string, returns the longest common prefix
      (using char-equal) of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-RIGHT-TRIM`

```lisp
COMMON-LISP:STRING-RIGHT-TRIM
  [symbol]

STRING-RIGHT-TRIM names a compiled function:
  Lambda-list: (CHAR-BAG STRING)
  Declared type: (FUNCTION (SEQUENCE (OR STRING SYMBOL CHARACTER))
                  (VALUES STRING &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-TRIM`

```lisp
COMMON-LISP:STRING-TRIM
  [symbol]

STRING-TRIM names a compiled function:
  Lambda-list: (CHAR-BAG STRING)
  Declared type: (FUNCTION (SEQUENCE (OR STRING SYMBOL CHARACTER))
                  (VALUES STRING &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING-UPCASE`

```lisp
COMMON-LISP:STRING-UPCASE
  [symbol]

STRING-UPCASE names a compiled function:
  Lambda-list: (STRING &KEY (START 0) END)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER) &KEY
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES SIMPLE-STRING &OPTIONAL))
  Derived type: (FUNCTION (T &KEY (:START . #1=(T)) (:END . #1#))
                 (VALUES SIMPLE-STRING &OPTIONAL))
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING/=`

```lisp
COMMON-LISP:STRING/=
  [symbol]

STRING/= names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is not lexicographically equal
      to the second string, returns the longest common prefix (using char=)
      of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING<`

```lisp
COMMON-LISP:STRING<
  [symbol]

STRING< names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is lexicographically less than
      the second string, returns the longest common prefix (using char=)
      of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING<=`

```lisp
COMMON-LISP:STRING<=
  [symbol]

STRING<= names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is lexicographically less than
      or equal to the second string, returns the longest common prefix
      (using char=) of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING=`

```lisp
COMMON-LISP:STRING=
  [symbol]

STRING= names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Given two strings (string1 and string2), and optional integers start1,
      start2, end1 and end2, compares characters in string1 to characters in
      string2 (using char=).
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING>`

```lisp
COMMON-LISP:STRING>
  [symbol]

STRING> names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is lexicographically greater than
      the second string, returns the longest common prefix (using char=)
      of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRING>=`

```lisp
COMMON-LISP:STRING>=
  [symbol]

STRING>= names a compiled function:
  Lambda-list: (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)
  Declared type: (FUNCTION
                  ((OR STRING SYMBOL CHARACTER)
                   (OR STRING SYMBOL CHARACTER) &KEY
                   (:START1 (UNSIGNED-BYTE 45))
                   (:END1 (OR NULL (UNSIGNED-BYTE 45)))
                   (:START2 (UNSIGNED-BYTE 45))
                   (:END2 (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Documentation:
    Given two strings, if the first string is lexicographically greater
      than or equal to the second string, returns the longest common prefix
      (using char=) of the two strings. Otherwise, returns ().
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

```

### `STRINGP`

```lisp
COMMON-LISP:STRINGP
  [symbol]

STRINGP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a STRING, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `SUBLIS`

```lisp
COMMON-LISP:SUBLIS
  [symbol]

SUBLIS names a compiled function:
  Lambda-list: (ALIST TREE &KEY KEY (TEST (FUNCTION EQL) TESTP)
                (TEST-NOT (FUNCTION EQL) NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST T &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Documentation:
    Substitute from ALIST into TREE nondestructively.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `SUBSEQ`

```lisp
COMMON-LISP:SUBSEQ
  [symbol]

SUBSEQ names a compiled function:
  Lambda-list: (SEQUENCE START &OPTIONAL END)
  Declared type: (FUNCTION
                  (SEQUENCE (UNSIGNED-BYTE 45) &OPTIONAL
                   (OR NULL (UNSIGNED-BYTE 45)))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (T #1=(UNSIGNED-BYTE 45) &OPTIONAL (OR NULL #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a copy of a subsequence of SEQUENCE starting with element number
       START and continuing to the end of SEQUENCE or the optional END.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

(SETF SUBSEQ) has a complex setf-expansion:
  Lambda-list: (SEQUENCE START &OPTIONAL END)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `SUBSETP`

```lisp
COMMON-LISP:SUBSETP
  [symbol]

SUBSETP names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if every element in LIST1 is also in LIST2.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `SUBST`

```lisp
COMMON-LISP:SUBST
  [symbol]

SUBST names a compiled function:
  Lambda-list: (NEW OLD TREE &KEY KEY (TEST (FUNCTION EQL) TESTP)
                (TEST-NOT (FUNCTION EQL) NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T T T &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Documentation:
    Substitutes new for subtrees matching old.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `SUBST-IF`

```lisp
COMMON-LISP:SUBST-IF
  [symbol]

SUBST-IF names a compiled function:
  Lambda-list: (NEW TEST TREE &KEY KEY)
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (T (OR FUNCTION SYMBOL) T &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Documentation:
    Substitutes new for subtrees for which test is true.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `SUBST-IF-NOT`

```lisp
COMMON-LISP:SUBST-IF-NOT
  [symbol]

SUBST-IF-NOT names a compiled function:
  Lambda-list: (NEW TEST TREE &KEY KEY)
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (T (OR FUNCTION SYMBOL) T &KEY
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES T &OPTIONAL))
  Documentation:
    Substitutes new for subtrees for which test is false.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `SUBSTITUTE`

```lisp
COMMON-LISP:SUBSTITUTE
  [symbol]

SUBSTITUTE names a compiled function:
  Lambda-list: (NEW OLD SEQUENCE &REST ARGS &KEY FROM-END
                (TEST (FUNCTION EQL)) (TEST-NOT NIL) (START 0)
                (COUNT NIL) (END NIL) (KEY NIL))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT :KEY)
  Declared type: (FUNCTION
                  (T T SEQUENCE &REST T &KEY (:FROM-END T)
                   (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:COUNT (OR NULL INTEGER))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (T T T &REST T &KEY (:FROM-END T)
                  (:TEST . #1=((OR FUNCTION SYMBOL))) (:TEST-NOT . #1#)
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:COUNT (OR NULL INTEGER))
                  (:KEY . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a sequence of the same kind as SEQUENCE with the same elements,
      except that all elements equal to OLD are replaced with NEW.
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `SUBSTITUTE-IF`

```lisp
COMMON-LISP:SUBSTITUTE-IF
  [symbol]

SUBSTITUTE-IF names a compiled function:
  Lambda-list: (NEW PREDICATE SEQUENCE &REST ARGS &KEY FROM-END
                (START 0) (END NIL) (COUNT NIL) (KEY NIL))
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (T (OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:COUNT (OR NULL INTEGER))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (T #1=(OR FUNCTION SYMBOL) T &REST T &KEY
                  (:FROM-END T) (:COUNT (OR NULL INTEGER))
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a sequence of the same kind as SEQUENCE with the same elements
      except that all elements satisfying the PRED are replaced with NEW.
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `SUBSTITUTE-IF-NOT`

```lisp
COMMON-LISP:SUBSTITUTE-IF-NOT
  [symbol]

SUBSTITUTE-IF-NOT names a compiled function:
  Lambda-list: (NEW PREDICATE SEQUENCE &REST ARGS &KEY FROM-END
                (START 0) (END NIL) (COUNT NIL) (KEY NIL))
  Dynamic-extent arguments: positional=(1), keyword=(:KEY)
  Declared type: (FUNCTION
                  (T (OR FUNCTION SYMBOL) SEQUENCE &REST T &KEY
                   (:FROM-END T) (:COUNT (OR NULL INTEGER))
                   (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45)))
                   (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES
                   (OR LIST (SIMPLE-ARRAY * (*))
                       SB-KERNEL:EXTENDED-SEQUENCE)
                   &OPTIONAL))
  Derived type: (FUNCTION
                 (T #1=(OR FUNCTION SYMBOL) T &REST T &KEY
                  (:FROM-END T) (:COUNT (OR NULL INTEGER))
                  (:START . #2=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #2#)) (:KEY #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return a sequence of the same kind as SEQUENCE with the same elements
      except that all elements not satisfying the PRED are replaced with NEW.
  Known attributes: call, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SEQ.LISP

```

### `SUBTYPEP`

```lisp
COMMON-LISP:SUBTYPEP
  [symbol]

SUBTYPEP names a compiled function:
  Lambda-list: (TYPE1 TYPE2 &OPTIONAL ENVIRONMENT)
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS)
                   (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS) &OPTIONAL
                   (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES BOOLEAN BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                 (VALUES T T &OPTIONAL))
  Documentation:
    Return two values indicating the relationship between type1 and type2.
      If values are T and T, type1 definitely is a subtype of type2.
      If values are NIL and T, type1 definitely is not a subtype of type2.
      If values are NIL and NIL, it couldn't be determined.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;TYPE.LISP

```

### `SVREF`

```lisp
COMMON-LISP:SVREF
  [symbol]

SVREF names a compiled function:
  Lambda-list: (SIMPLE-VECTOR INDEX)
  Declared type: (FUNCTION (SIMPLE-VECTOR (UNSIGNED-BYTE 45))
                  (VALUES T &OPTIONAL))
  Documentation:
    Return the INDEXth element of the given Simple-Vector.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

(SETF SVREF) names a compiled function:
  Lambda-list: (NEWVAL SIMPLE-VECTOR INDEX)
  Derived type: (FUNCTION (T SIMPLE-VECTOR (UNSIGNED-BYTE 45))
                 (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF SVREF) has setf-expansion: SB-KERNEL:%SVSET

```

### `(SETF SVREF)`

```lisp
COMMON-LISP-USER::|(SETF SVREF)|
  [symbol]

```

### `SXHASH`

```lisp
COMMON-LISP:SXHASH
  [symbol]

SXHASH names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION (T) (VALUES (UNSIGNED-BYTE 62) &OPTIONAL))
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;TARGET-SXHASH.LISP

```

### `SYMBOL-FUNCTION`

```lisp
COMMON-LISP:SYMBOL-FUNCTION
  [symbol]

SYMBOL-FUNCTION names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL) (VALUES FUNCTION &OPTIONAL))
  Documentation:
    Return SYMBOL's current function definition. Settable with SETF.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

(SETF SYMBOL-FUNCTION) names a compiled function:
  Lambda-list: (NEW-VALUE SYMBOL)
  Declared type: (FUNCTION (FUNCTION SYMBOL)
                  (VALUES FUNCTION &OPTIONAL))
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `(SETF SYMBOL-FUNCTION)`

```lisp
COMMON-LISP-USER::|(SETF SYMBOL-FUNCTION)|
  [symbol]

```

### `SYMBOL-NAME`

```lisp
COMMON-LISP:SYMBOL-NAME
  [symbol]

SYMBOL-NAME names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL) (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Return SYMBOL's name as a string.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `SYMBOL-PACKAGE`

```lisp
COMMON-LISP:SYMBOL-PACKAGE
  [symbol]

SYMBOL-PACKAGE names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL)
                  (VALUES (OR PACKAGE NULL) &OPTIONAL))
  Documentation:
    Return SYMBOL's home package, or NIL if none.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `SYMBOL-PLIST`

```lisp
COMMON-LISP:SYMBOL-PLIST
  [symbol]

SYMBOL-PLIST names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL) (VALUES LIST &OPTIONAL))
  Documentation:
    Return SYMBOL's property list.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;SYMBOL.LISP

(SETF SYMBOL-PLIST) names a compiled function:
  Lambda-list: (NEW-VALUE SYMBOL)
  Declared type: (FUNCTION (LIST SYMBOL) (VALUES LIST &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SYMBOL.LISP

```

### `(SETF SYMBOL-PLIST)`

```lisp
COMMON-LISP-USER::|(SETF SYMBOL-PLIST)|
  [symbol]

```

### `SYMBOL-VALUE`

```lisp
COMMON-LISP:SYMBOL-VALUE
  [symbol]

SYMBOL-VALUE names a compiled function:
  Lambda-list: (SYMBOL)
  Declared type: (FUNCTION (SYMBOL) (VALUES T &OPTIONAL))
  Documentation:
    Return SYMBOL's current bound value.
  Source file: SYS:SRC;CODE;SYMBOL.LISP

(SETF SYMBOL-VALUE) names a compiled function:
  Lambda-list: (NEWVAL SYMBOL)
  Derived type: (FUNCTION (T SYMBOL) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF SYMBOL-VALUE) has setf-expansion: SET

```

### `(SETF SYMBOL-VALUE)`

```lisp
COMMON-LISP-USER::|(SETF SYMBOL-VALUE)|
  [symbol]

```

### `SYMBOLP`

```lisp
COMMON-LISP:SYMBOLP
  [symbol]

SYMBOLP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a SYMBOL, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `SYNONYM-STREAM-SYMBOL`

```lisp
COMMON-LISP:SYNONYM-STREAM-SYMBOL
  [symbol]

SYNONYM-STREAM-SYMBOL names a compiled function:
  Lambda-list: (INSTANCE)
  Derived type: (FUNCTION (SYNONYM-STREAM) (VALUES SYMBOL &OPTIONAL))
  Source file: SYS:SRC;CODE;ANSI-STREAM.LISP

```

### `TAILP`

```lisp
COMMON-LISP:TAILP
  [symbol]

TAILP names a compiled function:
  Lambda-list: (OBJECT LIST)
  Declared type: (FUNCTION (T LIST) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is the same as some tail of LIST, otherwise
       returns false. LIST must be a proper list or a dotted list.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `TAN`

```lisp
COMMON-LISP:TAN
  [symbol]

TAN names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES (OR REAL COMPLEX) &OPTIONAL))
  Documentation:
    Return the tangent of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `TANH`

```lisp
COMMON-LISP:TANH
  [symbol]

TANH names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER)
                  (VALUES
                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
                       (COMPLEX DOUBLE-FLOAT))
                   &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the hyperbolic tangent of NUMBER.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;IRRAT.LISP

```

### `TENTH`

```lisp
COMMON-LISP:TENTH
  [symbol]

TENTH names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 10th object in a list or NIL if there is no 10th object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF TENTH) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF TENTH) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF TENTH)`

```lisp
COMMON-LISP-USER::|(SETF TENTH)|
  [symbol]

```

### `TERPRI`

```lisp
COMMON-LISP:TERPRI
  [symbol]

TERPRI names a compiled function:
  Lambda-list: (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  Declared type: (FUNCTION (&OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T) (VALUES NULL &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `THIRD`

```lisp
COMMON-LISP:THIRD
  [symbol]

THIRD names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) (VALUES T &OPTIONAL))
  Documentation:
    Return the 3rd object in a list or NIL if there is no 3rd object.
  Known attributes: foldable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

(SETF THIRD) names a compiled function:
  Lambda-list: (NEWVAL LIST)
  Derived type: (FUNCTION (T LIST) (VALUES T &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF THIRD) has a complex setf-expansion:
  Lambda-list: (LIST)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `(SETF THIRD)`

```lisp
COMMON-LISP-USER::|(SETF THIRD)|
  [symbol]

```

### `TRANSLATE-LOGICAL-PATHNAME`

```lisp
COMMON-LISP:TRANSLATE-LOGICAL-PATHNAME
  [symbol]

TRANSLATE-LOGICAL-PATHNAME names a compiled function:
  Lambda-list: (PATHNAME &KEY)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   &KEY)
                  (VALUES PATHNAME &OPTIONAL))
  Documentation:
    Translate PATHNAME to a physical pathname, which is returned.
  Known attributes: recursive
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `TRANSLATE-PATHNAME`

```lisp
COMMON-LISP:TRANSLATE-PATHNAME
  [symbol]

TRANSLATE-PATHNAME names a compiled function:
  Lambda-list: (SOURCE FROM-WILDNAME TO-WILDNAME &KEY)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   (OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   &KEY)
                  (VALUES PATHNAME &OPTIONAL))
  Documentation:
    Use the source pathname to translate the from-wildname's wild and
    unspecified elements into a completed to-pathname based on the to-wildname.
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `TREE-EQUAL`

```lisp
COMMON-LISP:TREE-EQUAL
  [symbol]

TREE-EQUAL names a compiled function:
  Lambda-list: (X Y &KEY (TEST NIL TESTP) (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (T T &KEY (:TEST (OR FUNCTION SYMBOL))
                   (:TEST-NOT (OR FUNCTION SYMBOL)))
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T T &KEY (:TEST . #1=(T)) (:TEST-NOT . #1#))
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if X and Y are isomorphic trees with identical leaves.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `TRUENAME`

```lisp
COMMON-LISP:TRUENAME
  [symbol]

TRUENAME names a compiled function:
  Lambda-list: (PATHSPEC)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM))
                  (VALUES PATHNAME &OPTIONAL))
  Documentation:
    If PATHSPEC is a pathname that names an existing file, return
    a pathname that denotes a canonicalized name for the file.  If
    pathspec is a stream associated with a file, return a pathname
    that denotes a canonicalized name for the file associated with
    the stream.
    
    An error of type FILE-ERROR is signalled if no such file exists
    or if the file system is such that a canonicalized file name
    cannot be determined or if the pathname is wild.
    
    Under Unix, the TRUENAME of a symlink that links to itself or to
    a file that doesn't exist is considered to be the name of the
    broken symlink itself.
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `TRUNCATE`

```lisp
COMMON-LISP:TRUNCATE
  [symbol]

TRUNCATE names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES INTEGER REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR NULL INTEGER) (OR REAL NULL) &OPTIONAL))
  Documentation:
    Return number (or number/divisor) as an integer, rounded toward 0.
      The second returned value is the remainder.
  Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```

### `TWO-WAY-STREAM-INPUT-STREAM`

```lisp
COMMON-LISP:TWO-WAY-STREAM-INPUT-STREAM
  [symbol]

TWO-WAY-STREAM-INPUT-STREAM names a compiled function:
  Lambda-list: (INSTANCE)
  Derived type: (FUNCTION (TWO-WAY-STREAM) (VALUES STREAM &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `TWO-WAY-STREAM-OUTPUT-STREAM`

```lisp
COMMON-LISP:TWO-WAY-STREAM-OUTPUT-STREAM
  [symbol]

TWO-WAY-STREAM-OUTPUT-STREAM names a compiled function:
  Lambda-list: (INSTANCE)
  Derived type: (FUNCTION (TWO-WAY-STREAM) (VALUES STREAM &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `TYPE-ERROR-DATUM`

```lisp
COMMON-LISP:TYPE-ERROR-DATUM
  [symbol]

TYPE-ERROR-DATUM names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `TYPE-ERROR-EXPECTED-TYPE`

```lisp
COMMON-LISP:TYPE-ERROR-EXPECTED-TYPE
  [symbol]

TYPE-ERROR-EXPECTED-TYPE names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `TYPE-OF`

```lisp
COMMON-LISP:TYPE-OF
  [symbol]

TYPE-OF names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T)
                  (VALUES (OR CONS SYMBOL CLASS) &OPTIONAL))
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the type of OBJECT.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `TYPEP`

```lisp
COMMON-LISP:TYPEP
  [symbol]

TYPEP names a compiled function:
  Lambda-list: (OBJECT TYPE &OPTIONAL ENVIRONMENT)
  Declared type: (FUNCTION
                  (T (OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS)
                   &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL)) *)
  Documentation:
    Is OBJECT of type TYPE?
  Known attributes: foldable
  Source file: SYS:SRC;CODE;TYPEP.LISP

```

### `UNBOUND-SLOT-INSTANCE`

```lisp
COMMON-LISP:UNBOUND-SLOT-INSTANCE
  [symbol]

UNBOUND-SLOT-INSTANCE names a compiled function:
  Lambda-list: (CONDITION)
  Declared type: (FUNCTION (T) *)
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `UNEXPORT`

```lisp
COMMON-LISP:UNEXPORT
  [symbol]

UNEXPORT names a compiled function:
  Lambda-list: (SYMBOLS &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Makes SYMBOLS no longer exported from PACKAGE.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `UNINTERN`

```lisp
COMMON-LISP:UNINTERN
  [symbol]

UNINTERN names a compiled function:
  Lambda-list: (SYMBOL &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  (SYMBOL &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Makes SYMBOL no longer present in PACKAGE. If SYMBOL was present then T is
    returned, otherwise NIL. If PACKAGE is SYMBOL's home package, then it is made
    uninterned.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `UNION`

```lisp
COMMON-LISP:UNION
  [symbol]

UNION names a compiled function:
  Lambda-list: (LIST1 LIST2 &KEY KEY (TEST NIL TESTP)
                (TEST-NOT NIL NOTP))
  Dynamic-extent arguments: keyword=(:KEY :TEST :TEST-NOT)
  Declared type: (FUNCTION
                  (LIST LIST &KEY (:TEST (OR FUNCTION SYMBOL))
                        (:TEST-NOT (OR FUNCTION SYMBOL))
                        (:KEY (OR FUNCTION SYMBOL)))
                  (VALUES LIST &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &KEY (:KEY . #1=(T)) (:TEST . #1#)
                  (:TEST-NOT . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Return the union of LIST1 and LIST2.
  Known attributes: call, foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;LIST.LISP

```

### `UNREAD-CHAR`

```lisp
COMMON-LISP:UNREAD-CHAR
  [symbol]

UNREAD-CHAR names a compiled function:
  Lambda-list: (CHARACTER &OPTIONAL (STREAM *STANDARD-INPUT*))
  Declared type: (FUNCTION (CHARACTER &OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES NULL &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `UNUSE-PACKAGE`

```lisp
COMMON-LISP:UNUSE-PACKAGE
  [symbol]

UNUSE-PACKAGE names a compiled function:
  Lambda-list: (PACKAGES-TO-UNUSE &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  ((OR STRING CONS SYMBOL CHARACTER PACKAGE) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Remove PACKAGES-TO-UNUSE from the USE list for PACKAGE.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `UPGRADED-ARRAY-ELEMENT-TYPE`

```lisp
COMMON-LISP:UPGRADED-ARRAY-ELEMENT-TYPE
  [symbol]

UPGRADED-ARRAY-ELEMENT-TYPE names a compiled function:
  Lambda-list: (SPEC &OPTIONAL ENVIRONMENT)
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS) &OPTIONAL
                   (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES (OR CONS SYMBOL) &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                 *)
  Documentation:
    Return the element type that will actually be used to implement an array
       with the specifier :ELEMENT-TYPE Spec.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;COMPILER;GENERIC;VM-TYPE.LISP

```

### `UPGRADED-COMPLEX-PART-TYPE`

```lisp
COMMON-LISP:UPGRADED-COMPLEX-PART-TYPE
  [symbol]

UPGRADED-COMPLEX-PART-TYPE names a compiled function:
  Lambda-list: (SPEC &OPTIONAL ENVIRONMENT)
  Declared type: (FUNCTION
                  ((OR CONS SYMBOL SB-KERNEL:CLASSOID CLASS) &OPTIONAL
                   (OR SB-C::ABSTRACT-LEXENV NULL))
                  (VALUES (OR CONS SYMBOL) &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL (OR SB-C::ABSTRACT-LEXENV NULL))
                 *)
  Documentation:
    Return the element type of the most specialized COMPLEX number type that
       can hold parts of type SPEC.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;COMPILER;GENERIC;VM-TYPE.LISP

```

### `UPPER-CASE-P`

```lisp
COMMON-LISP:UPPER-CASE-P
  [symbol]

UPPER-CASE-P names a compiled function:
  Lambda-list: (CHAR)
  Declared type: (FUNCTION (CHARACTER) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    The argument must be a character object; UPPER-CASE-P returns T if the
    argument is an upper-case character, NIL otherwise.
  Inline proclamation: MAYBE-INLINE (inline expansion available)
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;TARGET-CHAR.LISP

```

### `USE-PACKAGE`

```lisp
COMMON-LISP:USE-PACKAGE
  [symbol]

USE-PACKAGE names a compiled function:
  Lambda-list: (PACKAGES-TO-USE &OPTIONAL (PACKAGE (SANE-PACKAGE)))
  Declared type: (FUNCTION
                  ((OR STRING CONS SYMBOL CHARACTER PACKAGE) &OPTIONAL
                   (OR STRING SYMBOL CHARACTER PACKAGE))
                  (VALUES (MEMBER T) &OPTIONAL))
  Documentation:
    Add all the PACKAGES-TO-USE to the use list for PACKAGE so that the
    external symbols of the used packages are accessible as internal symbols in
    PACKAGE.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `USE-VALUE`

```lisp
COMMON-LISP:USE-VALUE
  [symbol]

USE-VALUE names a compiled function:
  Lambda-list: (VALUE &OPTIONAL CONDITION)
  Declared type: (FUNCTION (T &OPTIONAL (OR CONDITION NULL))
                  (VALUES NULL &OPTIONAL))
  Documentation:
    Transfer control and VALUE to a restart named USE-VALUE, or
    return NIL if none exists.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `USER-HOMEDIR-PATHNAME`

```lisp
COMMON-LISP:USER-HOMEDIR-PATHNAME
  [symbol]

USER-HOMEDIR-PATHNAME names a compiled function:
  Lambda-list: (&OPTIONAL HOST)
  Declared type: (FUNCTION (&OPTIONAL T) (VALUES PATHNAME &OPTIONAL))
  Documentation:
    Return the home directory of the user as a pathname. If the HOME
    environment variable has been specified, the directory it designates
    is returned; otherwise obtains the home directory from the operating
    system. HOST argument is ignored by SBCL.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;FILESYS.LISP

```

### `VALUES`

```lisp
COMMON-LISP:VALUES
  [symbol]

VALUES names a compiled function:
  Lambda-list: (&REST VALUES)
  Declared type: FUNCTION
  Derived type: (FUNCTION (&REST T) *)
  Documentation:
    Return all arguments, in order, as values.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;EVAL.LISP

(SETF VALUES) has a complex setf-expansion:
  Lambda-list: (&REST PLACES)
  (undocumented)
  Source file: SYS:SRC;CODE;DEFSETFS.LISP

```

### `VALUES-LIST`

```lisp
COMMON-LISP:VALUES-LIST
  [symbol]

VALUES-LIST names a compiled function:
  Lambda-list: (LIST)
  Declared type: (FUNCTION (LIST) *)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return all of the elements of LIST, in order, as values.
  Known attributes: foldable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;EVAL.LISP

```

### `VECTOR`

```lisp
COMMON-LISP:VECTOR
  [symbol]

VECTOR names a compiled function:
  Lambda-list: (&REST OBJECTS)
  Declared type: (FUNCTION * (VALUES SIMPLE-VECTOR &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES SIMPLE-VECTOR &OPTIONAL))
  Documentation:
    Construct a SIMPLE-VECTOR from the given objects.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

VECTOR names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:VECTOR>:
  Class precedence-list: VECTOR, ARRAY, SEQUENCE, T
  Direct superclasses: ARRAY, SEQUENCE
  Direct subclasses: BIT-VECTOR,
                     SB-KERNEL::SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-DOUBLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-FIXNUM,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-16,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-32,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-64,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-8,
                     SB-KERNEL::SIMPLE-ARRAY-SINGLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-15,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-16,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-2,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-31,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-32,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-4,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-63,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-64,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-7,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-8,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-FIXNUM,
                     SIMPLE-VECTOR, STRING, SB-KERNEL::VECTOR-NIL
  Sealed.
  No direct slots.

VECTOR names a primitive type-specifier:
  Lambda-list: (&OPTIONAL ELEMENT-TYPE SIZE)

```

### `VECTOR-POP`

```lisp
COMMON-LISP:VECTOR-POP
  [symbol]

VECTOR-POP names a compiled function:
  Lambda-list: (ARRAY)
  Declared type: (FUNCTION ((AND VECTOR (NOT SIMPLE-ARRAY)))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
  Documentation:
    Decrease the fill pointer by 1 and return the element pointed to by the
      new fill pointer.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `VECTOR-PUSH`

```lisp
COMMON-LISP:VECTOR-PUSH
  [symbol]

VECTOR-PUSH names a compiled function:
  Lambda-list: (NEW-ELEMENT ARRAY)
  Declared type: (FUNCTION (T (AND VECTOR (NOT SIMPLE-ARRAY)))
                  (VALUES (OR NULL (UNSIGNED-BYTE 45)) &OPTIONAL))
  Derived type: (FUNCTION (T T)
                 (VALUES (OR (MOD 35184372088831) NULL) &OPTIONAL))
  Documentation:
    Attempt to set the element of ARRAY designated by its fill pointer
       to NEW-ELEMENT, and increment the fill pointer by one. If the fill pointer is
       too large, NIL is returned, otherwise the index of the pushed element is
       returned.
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `VECTOR-PUSH-EXTEND`

```lisp
COMMON-LISP:VECTOR-PUSH-EXTEND
  [symbol]

VECTOR-PUSH-EXTEND names a compiled function:
  Lambda-list: (NEW-ELEMENT VECTOR &OPTIONAL MIN-EXTENSION)
  Declared type: (FUNCTION
                  (T (AND VECTOR (NOT SIMPLE-ARRAY)) &OPTIONAL
                   (INTEGER 1 35184372088831))
                  (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Derived type: (FUNCTION
                 (T T &OPTIONAL (OR NULL (INTEGER 1 35184372088831)))
                 (VALUES (UNSIGNED-BYTE 45) &OPTIONAL))
  Source file: SYS:SRC;CODE;ARRAY.LISP

```

### `VECTORP`

```lisp
COMMON-LISP:VECTORP
  [symbol]

VECTORP names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is a VECTOR, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate
  Source file: SYS:SRC;CODE;PRED.LISP

```

### `WARN`

```lisp
COMMON-LISP:WARN
  [symbol]

WARN names a compiled function:
  Lambda-list: (DATUM &REST ARGUMENTS)
  Declared type: (FUNCTION
                  ((OR STRING FUNCTION SYMBOL CONDITION
                       SB-PCL::CONDITION-CLASS)
                   &REST T)
                  (VALUES NULL &OPTIONAL))
  Derived type: (FUNCTION (T &REST T) *)
  Documentation:
    Warn about a situation by signalling a condition formed by DATUM and
       ARGUMENTS. While the condition is being signaled, a MUFFLE-WARNING restart
       exists that causes WARN to immediately return NIL.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;WARM-ERROR.LISP

```

### `WILD-PATHNAME-P`

```lisp
COMMON-LISP:WILD-PATHNAME-P
  [symbol]

WILD-PATHNAME-P names a compiled function:
  Lambda-list: (PATHNAME &OPTIONAL FIELD-KEY)
  Declared type: (FUNCTION
                  ((OR STRING PATHNAME SYNONYM-STREAM FILE-STREAM)
                   &OPTIONAL
                   (MEMBER :VERSION :TYPE :NAME :DIRECTORY :DEVICE
                           :HOST NIL))
                  (VALUES T &OPTIONAL))
  Documentation:
    Predicate for determining whether pathname contains any wildcards.
  Known attributes: recursive
  Source file: SYS:SRC;CODE;TARGET-PATHNAME.LISP

```

### `WRITE`

```lisp
COMMON-LISP:WRITE
  [symbol]

WRITE names a compiled function:
  Lambda-list: (OBJECT &KEY STREAM
                ((ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*)
                ((RADIX *PRINT-RADIX*) *PRINT-RADIX*)
                ((BASE *PRINT-BASE*) *PRINT-BASE*)
                ((CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*)
                ((PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*)
                ((LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*)
                ((LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*)
                ((CASE *PRINT-CASE*) *PRINT-CASE*)
                ((ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*)
                ((GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*)
                ((READABLY *PRINT-READABLY*) *PRINT-READABLY*)
                ((RIGHT-MARGIN *PRINT-RIGHT-MARGIN*)
                 *PRINT-RIGHT-MARGIN*)
                ((MISER-WIDTH *PRINT-MISER-WIDTH*) *PRINT-MISER-WIDTH*)
                ((LINES *PRINT-LINES*) *PRINT-LINES*)
                ((PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*)
                 *PRINT-PPRINT-DISPATCH*)
                ((SUPPRESS-ERRORS *SUPPRESS-PRINT-ERRORS*)
                 *SUPPRESS-PRINT-ERRORS*))
  Declared type: (FUNCTION
                  (T &KEY (:STREAM (OR STREAM BOOLEAN)) (:ESCAPE T)
                   (:RADIX T) (:BASE (INTEGER 2 36)) (:CIRCLE T)
                   (:PRETTY T) (:READABLY T)
                   (:LEVEL (OR UNSIGNED-BYTE NULL))
                   (:LENGTH (OR UNSIGNED-BYTE NULL)) (:CASE T)
                   (:ARRAY T) (:GENSYM T)
                   (:LINES (OR UNSIGNED-BYTE NULL))
                   (:RIGHT-MARGIN (OR UNSIGNED-BYTE NULL))
                   (:MISER-WIDTH (OR UNSIGNED-BYTE NULL))
                   (:PPRINT-DISPATCH T) (:SUPPRESS-ERRORS T))
                  (VALUES T &OPTIONAL))
  Derived type: (FUNCTION
                 (T &KEY (:STREAM . #1=(T)) (:ESCAPE . #1#)
                  (:RADIX . #1#) (:BASE (INTEGER 2 36)) (:CIRCLE . #1#)
                  (:PRETTY . #1#)
                  (:LEVEL . #2=((OR UNSIGNED-BYTE NULL)))
                  (:LENGTH . #2#)
                  (:CASE (MEMBER :CAPITALIZE :DOWNCASE :UPCASE))
                  (:ARRAY . #1#) (:GENSYM . #1#) (:READABLY . #1#)
                  (:RIGHT-MARGIN . #2#) (:MISER-WIDTH . #2#)
                  (:LINES . #2#)
                  (:PPRINT-DISPATCH SB-PRETTY:PPRINT-DISPATCH-TABLE)
                  (:SUPPRESS-ERRORS . #1#))
                 (VALUES T &OPTIONAL))
  Documentation:
    Output OBJECT to the specified stream, defaulting to *STANDARD-OUTPUT*.
  Known attributes: unwind, any
  Source file: SYS:SRC;CODE;PRINT.LISP

```

### `WRITE-BYTE`

```lisp
COMMON-LISP:WRITE-BYTE
  [symbol]

WRITE-BYTE names a compiled function:
  Lambda-list: (INTEGER STREAM)
  Declared type: (FUNCTION (INTEGER STREAM) (VALUES INTEGER &OPTIONAL))
  Derived type: (FUNCTION (T T) *)
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `WRITE-CHAR`

```lisp
COMMON-LISP:WRITE-CHAR
  [symbol]

WRITE-CHAR names a compiled function:
  Lambda-list: (CHARACTER &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  Declared type: (FUNCTION (CHARACTER &OPTIONAL (OR STREAM BOOLEAN))
                  (VALUES CHARACTER &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) *)
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `WRITE-LINE`

```lisp
COMMON-LISP:WRITE-LINE
  [symbol]

WRITE-LINE names a compiled function:
  Lambda-list: (STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY
                       (START 0) END)
  Declared type: (FUNCTION
                  (STRING &OPTIONAL (OR STREAM BOOLEAN) &KEY
                          (:START (UNSIGNED-BYTE 45))
                          (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES STRING &OPTIONAL))
  Derived type: (FUNCTION
                 (T &OPTIONAL T &KEY (:START . #1=(T)) (:END . #1#))
                 (VALUES ARRAY &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `WRITE-SEQUENCE`

```lisp
COMMON-LISP:WRITE-SEQUENCE
  [symbol]

WRITE-SEQUENCE names a compiled function:
  Lambda-list: (SEQ STREAM &KEY (START 0) (END NIL))
  Declared type: (FUNCTION
                  (SEQUENCE STREAM &KEY (:START (UNSIGNED-BYTE 45))
                   (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES SEQUENCE &OPTIONAL))
  Derived type: (FUNCTION
                 (T STREAM &KEY (:START . #1=((UNSIGNED-BYTE 45)))
                  (:END (OR NULL . #1#)))
                 (VALUES SEQUENCE &OPTIONAL))
  Documentation:
    Write the elements of SEQ bounded by START and END to STREAM.
  Known attributes: recursive
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `WRITE-STRING`

```lisp
COMMON-LISP:WRITE-STRING
  [symbol]

WRITE-STRING names a compiled function:
  Lambda-list: (STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY
                       (START 0) END)
  Declared type: (FUNCTION
                  (STRING &OPTIONAL (OR STREAM BOOLEAN) &KEY
                          (:START (UNSIGNED-BYTE 45))
                          (:END (OR NULL (UNSIGNED-BYTE 45))))
                  (VALUES STRING &OPTIONAL))
  Derived type: (FUNCTION
                 (T &OPTIONAL T &KEY (:START . #1=(T)) (:END . #1#))
                 (VALUES ARRAY &OPTIONAL))
  Source file: SYS:SRC;CODE;STREAM.LISP

```

### `WRITE-TO-STRING`

```lisp
COMMON-LISP:WRITE-TO-STRING
  [symbol]

WRITE-TO-STRING names a compiled function:
  Lambda-list: (OBJECT &KEY ((ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*)
                ((RADIX *PRINT-RADIX*) *PRINT-RADIX*)
                ((BASE *PRINT-BASE*) *PRINT-BASE*)
                ((CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*)
                ((PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*)
                ((LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*)
                ((LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*)
                ((CASE *PRINT-CASE*) *PRINT-CASE*)
                ((ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*)
                ((GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*)
                ((READABLY *PRINT-READABLY*) *PRINT-READABLY*)
                ((RIGHT-MARGIN *PRINT-RIGHT-MARGIN*)
                 *PRINT-RIGHT-MARGIN*)
                ((MISER-WIDTH *PRINT-MISER-WIDTH*) *PRINT-MISER-WIDTH*)
                ((LINES *PRINT-LINES*) *PRINT-LINES*)
                ((PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*)
                 *PRINT-PPRINT-DISPATCH*)
                ((SUPPRESS-ERRORS *SUPPRESS-PRINT-ERRORS*)
                 *SUPPRESS-PRINT-ERRORS*))
  Declared type: (FUNCTION
                  (T &KEY (:ESCAPE T) (:RADIX T) (:BASE (INTEGER 2 36))
                   (:CIRCLE T) (:PRETTY T) (:READABLY T)
                   (:LEVEL (OR UNSIGNED-BYTE NULL))
                   (:LENGTH (OR UNSIGNED-BYTE NULL)) (:CASE T)
                   (:ARRAY T) (:GENSYM T)
                   (:LINES (OR UNSIGNED-BYTE NULL))
                   (:RIGHT-MARGIN (OR UNSIGNED-BYTE NULL))
                   (:MISER-WIDTH (OR UNSIGNED-BYTE NULL))
                   (:PPRINT-DISPATCH T) (:SUPPRESS-ERRORS T))
                  (VALUES SIMPLE-STRING &OPTIONAL))
  Derived type: (FUNCTION
                 (T &KEY (:ESCAPE . #1=(T)) (:RADIX . #1#)
                  (:BASE (INTEGER 2 36)) (:CIRCLE . #1#)
                  (:PRETTY . #1#)
                  (:LEVEL . #2=((OR UNSIGNED-BYTE NULL)))
                  (:LENGTH . #2#)
                  (:CASE (MEMBER :CAPITALIZE :DOWNCASE :UPCASE))
                  (:ARRAY . #1#) (:GENSYM . #1#) (:READABLY . #1#)
                  (:RIGHT-MARGIN . #2#) (:MISER-WIDTH . #2#)
                  (:LINES . #2#)
                  (:PPRINT-DISPATCH SB-PRETTY:PPRINT-DISPATCH-TABLE)
                  (:SUPPRESS-ERRORS . #1#))
                 (VALUES SIMPLE-STRING &OPTIONAL))
  Documentation:
    Return the printed representation of OBJECT as a string.
  Known attributes: unsafely-flushable
  Source file: SYS:SRC;CODE;PRINT.LISP

```

### `Y-OR-N-P`

```lisp
COMMON-LISP:Y-OR-N-P
  [symbol]

Y-OR-N-P names a compiled function:
  Lambda-list: (&OPTIONAL FORMAT-STRING &REST ARGUMENTS)
  Declared type: (FUNCTION
                  (&OPTIONAL (OR STRING NULL FUNCTION) &REST T)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Y-OR-N-P prints the message, if any, and reads characters from
       *QUERY-IO* until the user enters y or Y as an affirmative, or either
       n or N as a negative answer. It asks again if you enter any other
       characters.
  Source file: SYS:SRC;CODE;QUERY.LISP

```

### `YES-OR-NO-P`

```lisp
COMMON-LISP:YES-OR-NO-P
  [symbol]

YES-OR-NO-P names a compiled function:
  Lambda-list: (&OPTIONAL FORMAT-STRING &REST ARGUMENTS)
  Declared type: (FUNCTION
                  (&OPTIONAL (OR STRING NULL FUNCTION) &REST T)
                  (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (&OPTIONAL T &REST T)
                 (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    YES-OR-NO-P is similar to Y-OR-N-P, except that it clears the
       input buffer, beeps, and uses READ-LINE to get the strings
       YES or NO.
  Source file: SYS:SRC;CODE;QUERY.LISP

```

### `ZEROP`

```lisp
COMMON-LISP:ZEROP
  [symbol]

ZEROP names a compiled function:
  Lambda-list: (NUMBER)
  Declared type: (FUNCTION (NUMBER) (VALUES BOOLEAN &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Is this number zero?
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

```


## generic function

### Symbol list

1. [ADD-METHOD](#add-method)
2. [ALLOCATE-INSTANCE](#allocate-instance)
3. [CHANGE-CLASS](#change-class)
4. [CLASS-NAME](#class-name)
5. [(SETF CLASS-NAME)](#(setf class-name))
6. [CLOSE](#close)
7. [COMPUTE-APPLICABLE-METHODS](#compute-applicable-methods)
8. [DESCRIBE-OBJECT](#describe-object)
9. [DOCUMENTATION](#documentation)
10. [(SETF DOCUMENTATION)](#(setf documentation))
11. [FIND-METHOD](#find-method)
12. [FUNCTION-KEYWORDS](#function-keywords)
13. [INITIALIZE-INSTANCE](#initialize-instance)
14. [INPUT-STREAM-P](#input-stream-p)
15. [INTERACTIVE-STREAM-P](#interactive-stream-p)
16. [MAKE-INSTANCE](#make-instance)
17. [MAKE-INSTANCES-OBSOLETE](#make-instances-obsolete)
18. [MAKE-LOAD-FORM](#make-load-form)
19. [METHOD-QUALIFIERS](#method-qualifiers)
20. [NO-APPLICABLE-METHOD](#no-applicable-method)
21. [NO-NEXT-METHOD](#no-next-method)
22. [OPEN-STREAM-P](#open-stream-p)
23. [OUTPUT-STREAM-P](#output-stream-p)
24. [PRINT-OBJECT](#print-object)
25. [REINITIALIZE-INSTANCE](#reinitialize-instance)
26. [REMOVE-METHOD](#remove-method)
27. [SHARED-INITIALIZE](#shared-initialize)
28. [SLOT-MISSING](#slot-missing)
29. [SLOT-UNBOUND](#slot-unbound)
30. [STREAM-ELEMENT-TYPE](#stream-element-type)
31. [UPDATE-INSTANCE-FOR-DIFFERENT-CLASS](#update-instance-for-different-class)
32. [UPDATE-INSTANCE-FOR-REDEFINED-CLASS](#update-instance-for-redefined-class)


### `ADD-METHOD`

```lisp
COMMON-LISP:ADD-METHOD
  [symbol]

ADD-METHOD names a generic function:
  Lambda-list: (GENERIC-FUNCTION METHOD)
  Argument precedence order: (GENERIC-FUNCTION METHOD)
  Derived type: (FUNCTION (T T) *)
  Method-combination: STANDARD
  Methods:
    (ADD-METHOD (STANDARD-GENERIC-FUNCTION METHOD))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `ALLOCATE-INSTANCE`

```lisp
COMMON-LISP:ALLOCATE-INSTANCE
  [symbol]

ALLOCATE-INSTANCE names a generic function:
  Lambda-list: (CLASS &REST INITARGS)
  Derived type: (FUNCTION (T &REST T) *)
  Method-combination: STANDARD
  Methods:
    (ALLOCATE-INSTANCE (STANDARD-CLASS))
    (ALLOCATE-INSTANCE (STRUCTURE-CLASS))
    (ALLOCATE-INSTANCE (CONDITION-CLASS))
    (ALLOCATE-INSTANCE (SYSTEM-CLASS))
    (ALLOCATE-INSTANCE (FUNCALLABLE-STANDARD-CLASS))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `CHANGE-CLASS`

```lisp
COMMON-LISP:CHANGE-CLASS
  [symbol]

CHANGE-CLASS names a generic function:
  Lambda-list: (INSTANCE NEW-CLASS-NAME &REST INITARGS &KEY
                &ALLOW-OTHER-KEYS)
  Argument precedence order: (INSTANCE NEW-CLASS-NAME)
  Derived type: (FUNCTION (T T &REST T &KEY &ALLOW-OTHER-KEYS) *)
  Method-combination: STANDARD
  Methods:
    (CHANGE-CLASS (STANDARD-OBJECT STANDARD-CLASS))
    (CHANGE-CLASS (FORWARD-REFERENCED-CLASS STANDARD-CLASS))
    (CHANGE-CLASS (T FORWARD-REFERENCED-CLASS))
    (CHANGE-CLASS (FUNCALLABLE-STANDARD-OBJECT
                   FUNCALLABLE-STANDARD-CLASS))
    (CHANGE-CLASS (STANDARD-OBJECT FUNCALLABLE-STANDARD-CLASS))
    (CHANGE-CLASS (FUNCALLABLE-STANDARD-OBJECT STANDARD-CLASS))
    (CHANGE-CLASS (T SYMBOL))
    (CHANGE-CLASS (METHOD T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `CLASS-NAME`

```lisp
COMMON-LISP:CLASS-NAME
  [symbol]

CLASS-NAME names a generic function:
  Lambda-list: (CLASS)
  Derived type: (FUNCTION (T) *)
  Known attributes: flushable, unsafely-flushable
  Method-combination: STANDARD
  Methods:
    (CLASS-NAME (CLASS))
      Documentation:
        automatically generated reader method

(SETF CLASS-NAME) names a generic function:
  Lambda-list: (NEW-VALUE CLASS)
  Argument precedence order: (NEW-VALUE CLASS)
  Derived type: (FUNCTION (T T) *)
  Method-combination: STANDARD
  Methods:
    ((SETF CLASS-NAME) (T T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `(SETF CLASS-NAME)`

```lisp
COMMON-LISP-USER::|(SETF CLASS-NAME)|
  [symbol]

```

### `CLOSE`

```lisp
COMMON-LISP:CLOSE
  [symbol]

CLOSE names a generic function:
  Lambda-list: (STREAM &KEY ABORT)
  Derived type: (FUNCTION (T &KEY (:ABORT T)) *)
  Documentation:
    Close the given STREAM. No more I/O may be performed, but
      inquiries may still be made. If :ABORT is true, an attempt is made
      to clean up the side effects of having created the stream.
  Method-combination: STANDARD
  Methods:
    (CLOSE (IO-STREAM))
    (CLOSE (FLEXI-STREAM))
      Documentation:
        Closes the flexi stream by closing the underlying `real'
        stream.
    (CLOSE (FUNDAMENTAL-STREAM))
    (CLOSE (ANSI-STREAM))
  Source file: SYS:SRC;PCL;GRAY-STREAMS.LISP

```

### `COMPUTE-APPLICABLE-METHODS`

```lisp
COMMON-LISP:COMPUTE-APPLICABLE-METHODS
  [symbol]

COMPUTE-APPLICABLE-METHODS names a generic function:
  Lambda-list: (GENERIC-FUNCTION ARGUMENTS)
  Argument precedence order: (GENERIC-FUNCTION ARGUMENTS)
  Derived type: (FUNCTION (T T) *)
  Method-combination: STANDARD
  Methods:
    (COMPUTE-APPLICABLE-METHODS (GENERIC-FUNCTION T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `DESCRIBE-OBJECT`

```lisp
COMMON-LISP:DESCRIBE-OBJECT
  [symbol]

DESCRIBE-OBJECT names a generic function:
  Lambda-list: (OBJECT STREAM)
  Argument precedence order: (OBJECT STREAM)
  Derived type: (FUNCTION (T T) *)
  Method-combination: STANDARD
  Methods:
    (DESCRIBE-OBJECT :AFTER (HEADER T))
    (DESCRIBE-OBJECT (PACKAGE T))
    (DESCRIBE-OBJECT (SYMBOL T))
    (DESCRIBE-OBJECT (HASH-TABLE T))
    (DESCRIBE-OBJECT (ARRAY T))
    (DESCRIBE-OBJECT (CHARACTER T))
    (DESCRIBE-OBJECT (PATHNAME T))
    (DESCRIBE-OBJECT (SLOT-OBJECT T))
    (DESCRIBE-OBJECT (CLASS T))
    (DESCRIBE-OBJECT (FUNCTION T))
    (DESCRIBE-OBJECT (CONS T))
    (DESCRIBE-OBJECT (T T))
  Source file: SYS:SRC;CODE;DESCRIBE.LISP

```

### `DOCUMENTATION`

```lisp
COMMON-LISP:DOCUMENTATION
  [symbol]

DOCUMENTATION names a generic function:
  Lambda-list: (OBJECT DOC-TYPE)
  Argument precedence order: (DOC-TYPE OBJECT)
  Derived type: (FUNCTION (T T) *)
  Documentation:
    Return the documentation string of Doc-Type for X, or NIL if none
    exists. System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE, SETF, and T.
    
    Function documentation is stored separately for function names and objects:
    DEFUN, LAMBDA, &co create function objects with the specified documentation
    strings.
    
     (SETF (DOCUMENTATION NAME 'FUNCTION) STRING)
    
    sets the documentation string stored under the specified name, and
    
     (SETF (DOCUMENTATION FUNC T) STRING)
    
    sets the documentation string stored in the function object.
    
     (DOCUMENTATION NAME 'FUNCTION)
    
    returns the documentation stored under the function name if any, and
    falls back on the documentation in the function object if necessary.
  Method-combination: STANDARD
  Methods:
    (DOCUMENTATION (SYMBOL (EQL OPTIMIZE)))
    (DOCUMENTATION (PACKAGE (EQL T)))
    (DOCUMENTATION (STANDARD-SLOT-DEFINITION (EQL T)))
    (DOCUMENTATION (SYMBOL (EQL VARIABLE)))
    (DOCUMENTATION (SYMBOL (EQL STRUCTURE)))
    (DOCUMENTATION (SYMBOL (EQL TYPE)))
    (DOCUMENTATION (CONDITION-CLASS (EQL TYPE)))
    (DOCUMENTATION (CONDITION-CLASS (EQL T)))
    (DOCUMENTATION (CLASS (EQL TYPE)))
    (DOCUMENTATION (CLASS (EQL T)))
    (DOCUMENTATION (STRUCTURE-CLASS (EQL TYPE)))
    (DOCUMENTATION (STRUCTURE-CLASS (EQL T)))
    (DOCUMENTATION (STANDARD-METHOD (EQL T)))
    (DOCUMENTATION (SYMBOL (EQL METHOD-COMBINATION)))
    (DOCUMENTATION (METHOD-COMBINATION (EQL METHOD-COMBINATION)))
    (DOCUMENTATION (METHOD-COMBINATION (EQL T)))
    (DOCUMENTATION (SYMBOL (EQL SETF)))
    (DOCUMENTATION (SYMBOL (EQL COMPILER-MACRO)))
    (DOCUMENTATION (SYMBOL (EQL FUNCTION)))
    (DOCUMENTATION (LIST (EQL FUNCTION)))
    (DOCUMENTATION (LIST (EQL COMPILER-MACRO)))
    (DOCUMENTATION (FUNCTION (EQL FUNCTION)))
    (DOCUMENTATION (FUNCTION (EQL T)))
    (DOCUMENTATION :AROUND (T T))
    (DOCUMENTATION (T T))
  Source file: SYS:SRC;PCL;DOCUMENTATION.LISP

(SETF DOCUMENTATION) names a generic function:
  Lambda-list: (NEW-VALUE OBJECT DOC-TYPE)
  Argument precedence order: (DOC-TYPE OBJECT NEW-VALUE)
  Derived type: (FUNCTION (T T T) *)
  Method-combination: STANDARD
  Methods:
    ((SETF DOCUMENTATION) (T PACKAGE (EQL T)))
    ((SETF DOCUMENTATION) (T STANDARD-SLOT-DEFINITION (EQL T)))
    ((SETF DOCUMENTATION) (T SYMBOL (EQL VARIABLE)))
    ((SETF DOCUMENTATION) (T SYMBOL (EQL STRUCTURE)))
    ((SETF DOCUMENTATION) (T SYMBOL (EQL TYPE)))
    ((SETF DOCUMENTATION) (T CONDITION-CLASS (EQL TYPE)))
    ((SETF DOCUMENTATION) (T CONDITION-CLASS (EQL T)))
    ((SETF DOCUMENTATION) (T CLASS (EQL TYPE)))
    ((SETF DOCUMENTATION) (T CLASS (EQL T)))
    ((SETF DOCUMENTATION) (T STRUCTURE-CLASS (EQL TYPE)))
    ((SETF DOCUMENTATION) (T STRUCTURE-CLASS (EQL T)))
    ((SETF DOCUMENTATION) (T STANDARD-METHOD (EQL T)))
    ((SETF DOCUMENTATION) (T SYMBOL (EQL METHOD-COMBINATION)))
    ((SETF DOCUMENTATION) (T METHOD-COMBINATION
                           (EQL METHOD-COMBINATION)))
    ((SETF DOCUMENTATION) (T METHOD-COMBINATION (EQL T)))
    ((SETF DOCUMENTATION) (T SYMBOL (EQL SETF)))
    ((SETF DOCUMENTATION) (T SYMBOL (EQL COMPILER-MACRO)))
    ((SETF DOCUMENTATION) (T SYMBOL (EQL FUNCTION)))
    ((SETF DOCUMENTATION) (T LIST (EQL COMPILER-MACRO)))
    ((SETF DOCUMENTATION) (T LIST (EQL FUNCTION)))
    ((SETF DOCUMENTATION) (T FUNCTION (EQL FUNCTION)))
    ((SETF DOCUMENTATION) (T FUNCTION (EQL T)))
    ((SETF DOCUMENTATION) (T T T))
    ((SETF DOCUMENTATION) :AROUND (T (EQL NIL) T))
  Source file: SYS:SRC;PCL;DOCUMENTATION.LISP

```

### `(SETF DOCUMENTATION)`

```lisp
COMMON-LISP-USER::|(SETF DOCUMENTATION)|
  [symbol]

```

### `FIND-METHOD`

```lisp
COMMON-LISP:FIND-METHOD
  [symbol]

FIND-METHOD names a generic function:
  Lambda-list: (GENERIC-FUNCTION QUALIFIERS SPECIALIZERS &OPTIONAL
                ERRORP)
  Argument precedence order: (GENERIC-FUNCTION QUALIFIERS SPECIALIZERS)
  Derived type: (FUNCTION (T T T &OPTIONAL T) *)
  Method-combination: STANDARD
  Methods:
    (FIND-METHOD (STANDARD-GENERIC-FUNCTION T T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `FUNCTION-KEYWORDS`

```lisp
COMMON-LISP:FUNCTION-KEYWORDS
  [symbol]

FUNCTION-KEYWORDS names a generic function:
  Lambda-list: (METHOD)
  Derived type: (FUNCTION (T) *)
  Method-combination: STANDARD
  Methods:
    (FUNCTION-KEYWORDS (STANDARD-METHOD))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `INITIALIZE-INSTANCE`

```lisp
COMMON-LISP:INITIALIZE-INSTANCE
  [symbol]

INITIALIZE-INSTANCE names a generic function:
  Lambda-list: (INSTANCE &REST INITARGS &KEY &ALLOW-OTHER-KEYS)
  Derived type: (FUNCTION
                 (T &REST T &KEY (:SYSTEM T) (:OTHER-SYSTEMS T)
                  (:NAME T) (:INITFORM T) (:INITFUNCTION T) (:TYPE T)
                  (:ALLOCATION T) (:INITARGS T) (:DOCUMENTATION T)
                  (:READERS T) (:WRITERS T) (:LAMBDA-LIST T)
                  (:ARGUMENT-PRECEDENCE-ORDER T) &ALLOW-OTHER-KEYS)
                 *)
  Method-combination: STANDARD
  Methods:
    (INITIALIZE-INSTANCE :AFTER (FLEXI-STREAM))
      Documentation:
        Makes sure the EXTERNAL-FORMAT and ELEMENT-TYPE slots contain
        reasonable values.
    (INITIALIZE-INSTANCE :AFTER (FLEXI-8-BIT-FORMAT))
      Documentation:
        Sets the fixed encoding/decoding tables for this particular
        external format.
    (INITIALIZE-INSTANCE :AFTER (FILTERED-SEQUENTIAL-PLAN))
    (INITIALIZE-INSTANCE :BEFORE (NON-PROPAGATING-OPERATION))
    (INITIALIZE-INSTANCE :BEFORE (OPERATION))
    (INITIALIZE-INSTANCE :AFTER (OPERATION))
    (INITIALIZE-INSTANCE (SLOT-OBJECT))
    (INITIALIZE-INSTANCE :BEFORE (SLOT-DEFINITION))
    (INITIALIZE-INSTANCE :BEFORE (DIRECT-SLOT-DEFINITION))
    (INITIALIZE-INSTANCE :AFTER (EFFECTIVE-SLOT-DEFINITION))
    (INITIALIZE-INSTANCE (SYSTEM-CLASS))
    (INITIALIZE-INSTANCE :AFTER (STANDARD-GENERIC-FUNCTION))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `INPUT-STREAM-P`

```lisp
COMMON-LISP:INPUT-STREAM-P
  [symbol]

INPUT-STREAM-P names a generic function:
  Lambda-list: (STREAM)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Can STREAM perform input operations?
  Method-combination: STANDARD
  Methods:
    (INPUT-STREAM-P (FUNDAMENTAL-INPUT-STREAM))
    (INPUT-STREAM-P (FUNDAMENTAL-STREAM))
    (INPUT-STREAM-P (ANSI-STREAM))
  Source file: SYS:SRC;PCL;GRAY-STREAMS.LISP

```

### `INTERACTIVE-STREAM-P`

```lisp
COMMON-LISP:INTERACTIVE-STREAM-P
  [symbol]

INTERACTIVE-STREAM-P names a generic function:
  Lambda-list: (STREAM)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Is STREAM an interactive stream?
  Method-combination: STANDARD
  Methods:
    (INTERACTIVE-STREAM-P (FUNDAMENTAL-STREAM))
    (INTERACTIVE-STREAM-P (ANSI-STREAM))
  Source file: SYS:SRC;PCL;GRAY-STREAMS.LISP

```

### `MAKE-INSTANCE`

```lisp
COMMON-LISP:MAKE-INSTANCE
  [symbol]

MAKE-INSTANCE names a generic function:
  Lambda-list: (CLASS &REST INITARGS &KEY &ALLOW-OTHER-KEYS)
  Derived type: (FUNCTION (T &REST T &KEY &ALLOW-OTHER-KEYS) *)
  Method-combination: STANDARD
  Methods:
    (MAKE-INSTANCE (SYMBOL))
    (MAKE-INSTANCE (CLASS))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `MAKE-INSTANCES-OBSOLETE`

```lisp
COMMON-LISP:MAKE-INSTANCES-OBSOLETE
  [symbol]

MAKE-INSTANCES-OBSOLETE names a generic function:
  Lambda-list: (CLASS)
  Derived type: (FUNCTION (T) *)
  Method-combination: STANDARD
  Methods:
    (MAKE-INSTANCES-OBSOLETE (STD-CLASS))
    (MAKE-INSTANCES-OBSOLETE (SYMBOL))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `MAKE-LOAD-FORM`

```lisp
COMMON-LISP:MAKE-LOAD-FORM
  [symbol]

MAKE-LOAD-FORM names a generic function:
  Lambda-list: (OBJECT &OPTIONAL ENVIRONMENT)
  Derived type: (FUNCTION (T &OPTIONAL T) *)
  Method-combination: STANDARD
  Methods:
    (MAKE-LOAD-FORM (EXTERNAL-FORMAT))
      Documentation:
        Defines a way to reconstruct external formats.  Needed for OpenMCL.
    (MAKE-LOAD-FORM (PROTOTYPE))
    (MAKE-LOAD-FORM (CONDITION))
    (MAKE-LOAD-FORM (STANDARD-OBJECT))
    (MAKE-LOAD-FORM (STRUCTURE-OBJECT))
    (MAKE-LOAD-FORM (ALIEN-TYPE))
    (MAKE-LOAD-FORM (LAYOUT))
    (MAKE-LOAD-FORM (CLASS))
    (MAKE-LOAD-FORM (COMMA))
    (MAKE-LOAD-FORM (DEFSTRUCT-DESCRIPTION))
    (MAKE-LOAD-FORM (CLASSOID-CELL))
    (MAKE-LOAD-FORM (HEAP-ALIEN-INFO))
    (MAKE-LOAD-FORM (LVAR))
    (MAKE-LOAD-FORM (NLX-INFO))
    (MAKE-LOAD-FORM (LEAF))
    (MAKE-LOAD-FORM (VOP-INFO))
    (MAKE-LOAD-FORM (ALIEN-RECORD-FIELD))
    (MAKE-LOAD-FORM (LOCAL-ALIEN-INFO))
    (MAKE-LOAD-FORM (RANDOM-STATE))
    (MAKE-LOAD-FORM (DEFINITION-SOURCE-LOCATION))
    (MAKE-LOAD-FORM (DEFSTRUCT-SLOT-DESCRIPTION))
    (MAKE-LOAD-FORM (FMT-CONTROL-PROXY))
    (MAKE-LOAD-FORM (LOCAL-CALL-CONTEXT))
    (MAKE-LOAD-FORM (RESTART-LOCATION))
    (MAKE-LOAD-FORM (HASH-TABLE))
    (MAKE-LOAD-FORM (PATHNAME))
    (MAKE-LOAD-FORM (TRACE-INFO))
  Source file: SYS:SRC;PCL;ENV.LISP

```

### `METHOD-QUALIFIERS`

```lisp
COMMON-LISP:METHOD-QUALIFIERS
  [symbol]

METHOD-QUALIFIERS names a generic function:
  Lambda-list: (M)
  Derived type: (FUNCTION (T) *)
  Method-combination: STANDARD
  Methods:
    (METHOD-QUALIFIERS (STANDARD-METHOD))
      Documentation:
        automatically generated reader method

```

### `NO-APPLICABLE-METHOD`

```lisp
COMMON-LISP:NO-APPLICABLE-METHOD
  [symbol]

NO-APPLICABLE-METHOD names a generic function:
  Lambda-list: (GENERIC-FUNCTION &REST ARGS)
  Derived type: (FUNCTION (T &REST T) *)
  Method-combination: STANDARD
  Methods:
    (NO-APPLICABLE-METHOD (STREAM-FUNCTION))
    (NO-APPLICABLE-METHOD (CNM-ARGS-CHECKER))
    (NO-APPLICABLE-METHOD (T))

```

### `NO-NEXT-METHOD`

```lisp
COMMON-LISP:NO-NEXT-METHOD
  [symbol]

NO-NEXT-METHOD names a generic function:
  Lambda-list: (GENERIC-FUNCTION METHOD &REST ARGS)
  Argument precedence order: (GENERIC-FUNCTION METHOD)
  Derived type: (FUNCTION (T T &REST T) *)
  Method-combination: STANDARD
  Methods:
    (NO-NEXT-METHOD (STANDARD-GENERIC-FUNCTION STANDARD-METHOD))

```

### `OPEN-STREAM-P`

```lisp
COMMON-LISP:OPEN-STREAM-P
  [symbol]

OPEN-STREAM-P names a generic function:
  Lambda-list: (STREAM)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return true if STREAM is not closed. A default method is provided
      by class FUNDAMENTAL-STREAM which returns true if CLOSE has not been
      called on the stream.
  Method-combination: STANDARD
  Methods:
    (OPEN-STREAM-P (FLEXI-STREAM))
      Documentation:
        A flexi stream is open if its underlying stream is open.
    (OPEN-STREAM-P (FUNDAMENTAL-STREAM))
    (OPEN-STREAM-P (ANSI-STREAM))
  Source file: SYS:SRC;PCL;GRAY-STREAMS.LISP

```

### `OUTPUT-STREAM-P`

```lisp
COMMON-LISP:OUTPUT-STREAM-P
  [symbol]

OUTPUT-STREAM-P names a generic function:
  Lambda-list: (STREAM)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Can STREAM perform output operations?
  Method-combination: STANDARD
  Methods:
    (OUTPUT-STREAM-P (FUNDAMENTAL-OUTPUT-STREAM))
    (OUTPUT-STREAM-P (FUNDAMENTAL-STREAM))
    (OUTPUT-STREAM-P (ANSI-STREAM))
  Source file: SYS:SRC;PCL;GRAY-STREAMS.LISP

```

### `PRINT-OBJECT`

```lisp
COMMON-LISP:PRINT-OBJECT
  [symbol]

PRINT-OBJECT names a generic function:
  Lambda-list: (OBJECT STREAM)
  Argument precedence order: (OBJECT STREAM)
  Derived type: (FUNCTION (T T) *)
  Method-combination: STANDARD
  Methods:
    (PRINT-OBJECT (EXTERNAL-FORMAT T))
      Documentation:
        How an EXTERNAL-FORMAT object is rendered.  Uses
        NORMALIZE-EXTERNAL-FORMAT.
    (PRINT-OBJECT (THREAD T))
    (PRINT-OBJECT (RECURSIVE-LOCK T))
    (PRINT-OBJECT (LOCK T))
    (PRINT-OBJECT (ATOMIC-INTEGER T))
    (PRINT-OBJECT (BUNDLE T))
    (PRINT-OBJECT (CLIENT-INFO T))
    (PRINT-OBJECT (CLIENT-FILE-INFO T))
    (PRINT-OBJECT (LOAD-STRATEGY T))
    (PRINT-OBJECT (SYSTEM T))
    (PRINT-OBJECT (RELEASE T))
    (PRINT-OBJECT (DIST T))
    (PRINT-OBJECT (HASH-TABLE-BUCKET T))
    (PRINT-OBJECT (RECORD-POINTER T))
    (PRINT-OBJECT (URL T))
    (PRINT-OBJECT (HEADER T))
    (PRINT-OBJECT (SOCKET T))
    (PRINT-OBJECT (ACTION-STATUS T))
    (PRINT-OBJECT (FORCING T))
    (PRINT-OBJECT (MISSING-COMPONENT-OF-VERSION T))
    (PRINT-OBJECT (MISSING-COMPONENT T))
    (PRINT-OBJECT (MISSING-DEPENDENCY T))
    (PRINT-OBJECT (OPERATION T))
    (PRINT-OBJECT (COMPONENT T))
    (PRINT-OBJECT (DEPRECATED-FUNCTION-CONDITION T))
    (PRINT-OBJECT (TIMER T))
    (PRINT-OBJECT (PRIORITY-QUEUE T))
    (PRINT-OBJECT (PROCESS T))
    (PRINT-OBJECT (INTERPRETED-FUNCTION T))
    (PRINT-OBJECT (ENV T))
    (PRINT-OBJECT :AROUND (REFERENCE-CONDITION T))
    (PRINT-OBJECT (CELL-ERROR T))
    (PRINT-OBJECT (TYPE-ERROR T))
    (PRINT-OBJECT (POLICY T))
    (PRINT-OBJECT (SPECIALIZER-WITH-OBJECT T))
    (PRINT-OBJECT (CLASS-PRECEDENCE-DESCRIPTION T))
    (PRINT-OBJECT (CTOR T))
    (PRINT-OBJECT (DFUN-INFO T))
    (PRINT-OBJECT (CACHE T))
    (PRINT-OBJECT (STANDARD-GENERIC-FUNCTION T))
    (PRINT-OBJECT (SLOT-DEFINITION T))
    (PRINT-OBJECT (CLASS T))
    (PRINT-OBJECT (STANDARD-METHOD-COMBINATION T))
    (PRINT-OBJECT (STANDARD-ACCESSOR-METHOD T))
    (PRINT-OBJECT (STANDARD-METHOD T))
    (PRINT-OBJECT (FUNCALLABLE-STANDARD-OBJECT T))
    (PRINT-OBJECT (STANDARD-OBJECT T))
    (PRINT-OBJECT (PACKED-INFO T))
    (PRINT-OBJECT (STREAM T))
    (PRINT-OBJECT (ROBINHOOD-HASHSET T))
    (PRINT-OBJECT (TYPE-CLASS T))
    (PRINT-OBJECT (KEY-INFO T))
    (PRINT-OBJECT (CTYPE T))
    (PRINT-OBJECT (CLASSOID T))
    (PRINT-OBJECT (CLASSOID-CELL T))
    (PRINT-OBJECT (RESTART T))
    (PRINT-OBJECT (DEPRECATION-CONDITION T))
    (PRINT-OBJECT :AFTER (EARLY-DEPRECATION-WARNING T))
    (PRINT-OBJECT :AFTER (LATE-DEPRECATION-WARNING T))
    (PRINT-OBJECT :AFTER (FINAL-DEPRECATION-WARNING T))
    (PRINT-OBJECT (CONDITION T))
    (PRINT-OBJECT (FUN-INFO T))
    (PRINT-OBJECT (INFO-HASHTABLE T))
    (PRINT-OBJECT (META-INFO T))
    (PRINT-OBJECT (SSET T))
    (PRINT-OBJECT (LEXENV T))
    (PRINT-OBJECT (CTRAN T))
    (PRINT-OBJECT (LVAR-ANNOTATION T))
    (PRINT-OBJECT (LVAR T))
    (PRINT-OBJECT (CLOOP T))
    (PRINT-OBJECT (CBLOCK T))
    (PRINT-OBJECT (COMPONENT T))
    (PRINT-OBJECT (CLEANUP T))
    (PRINT-OBJECT (ENVIRONMENT T))
    (PRINT-OBJECT (TAIL-SET T))
    (PRINT-OBJECT (NLX-INFO T))
    (PRINT-OBJECT (CONSTANT T))
    (PRINT-OBJECT (GLOBAL-VAR T))
    (PRINT-OBJECT (DEFINED-FUN T))
    (PRINT-OBJECT (FUNCTIONAL T))
    (PRINT-OBJECT (CLAMBDA T))
    (PRINT-OBJECT (OPTIONAL-DISPATCH T))
    (PRINT-OBJECT (ARG-INFO T))
    (PRINT-OBJECT (LAMBDA-VAR T))
    (PRINT-OBJECT (REF T))
    (PRINT-OBJECT (CIF T))
    (PRINT-OBJECT (JUMP-TABLE T))
    (PRINT-OBJECT (CSET T))
    (PRINT-OBJECT (COMBINATION T))
    (PRINT-OBJECT (MV-COMBINATION T))
    (PRINT-OBJECT (BIND T))
    (PRINT-OBJECT (CRETURN T))
    (PRINT-OBJECT (CAST T))
    (PRINT-OBJECT (ENTRY T))
    (PRINT-OBJECT (EXIT T))
    (PRINT-OBJECT (ENCLOSE T))
    (PRINT-OBJECT (CDYNAMIC-EXTENT T))
    (PRINT-OBJECT (UNDEFINED-WARNING T))
    (PRINT-OBJECT (ARGUMENT-MISMATCH-WARNING T))
    (PRINT-OBJECT (LABEL T))
    (PRINT-OBJECT (PRIMITIVE-TYPE T))
    (PRINT-OBJECT (IR2-BLOCK T))
    (PRINT-OBJECT (IR2-LVAR T))
    (PRINT-OBJECT (IR2-ENVIRONMENT T))
    (PRINT-OBJECT (RETURN-INFO T))
    (PRINT-OBJECT (IR2-NLX-INFO T))
    (PRINT-OBJECT (TEMPLATE T))
    (PRINT-OBJECT (VOP T))
    (PRINT-OBJECT (TN-REF T))
    (PRINT-OBJECT (STORAGE-CLASS T))
    (PRINT-OBJECT (TN T))
    (PRINT-OBJECT (GLOBAL-CONFLICTS T))
    (PRINT-OBJECT (VOP-PARSE T))
    (PRINT-OBJECT (OPERAND-PARSE T))
    (PRINT-OBJECT (ALIEN-TYPE-CLASS T))
    (PRINT-OBJECT (ALIEN-RECORD-FIELD T))
    (PRINT-OBJECT (HEAP-ALIEN-INFO T))
    (PRINT-OBJECT (LOCAL-ALIEN-INFO T))
    (PRINT-OBJECT (ALIEN-TYPE T))
    (PRINT-OBJECT (ALIEN-VALUE T))
    (PRINT-OBJECT (ARENA T))
    (PRINT-OBJECT (RANDOM-STATE T))
    (PRINT-OBJECT (LAYOUT T))
    (PRINT-OBJECT (HOST T))
    (PRINT-OBJECT (FILE-INFO T))
    (PRINT-OBJECT (SOURCE-INFO T))
    (PRINT-OBJECT (DEFSTRUCT-DESCRIPTION T))
    (PRINT-OBJECT (DEFSTRUCT-SLOT-DESCRIPTION T))
    (PRINT-OBJECT (TRANSFORM T))
    (PRINT-OBJECT (FD-STREAM T))
    (PRINT-OBJECT (SYNONYM-STREAM T))
    (PRINT-OBJECT (TWO-WAY-STREAM T))
    (PRINT-OBJECT (CONCATENATED-STREAM T))
    (PRINT-OBJECT (ECHO-STREAM T))
    (PRINT-OBJECT (STUB-STREAM T))
    (PRINT-OBJECT (SEGMENT T))
    (PRINT-OBJECT (INSTRUCTION T))
    (PRINT-OBJECT (STMT T))
    (PRINT-OBJECT (FASL-OUTPUT T))
    (PRINT-OBJECT (CORE-OBJECT T))
    (PRINT-OBJECT (COMPILER-ERROR-CONTEXT T))
    (PRINT-OBJECT (DISASSEM-STATE T))
    (PRINT-OBJECT (EA T))
    (PRINT-OBJECT (VERTEX T))
    (PRINT-OBJECT (HANDLER T))
    (PRINT-OBJECT (BINARY-NODE T))
    (PRINT-OBJECT (UNARY-NODE T))
    (PRINT-OBJECT (SYMBOL-TABLE T))
    (PRINT-OBJECT (PACKAGE T))
    (PRINT-OBJECT (HASH-TABLE T))
    (PRINT-OBJECT (LINKED-LIST T))
    (PRINT-OBJECT (LIST-NODE T))
    (PRINT-OBJECT (SPLIT-ORDERED-LIST T))
    (PRINT-OBJECT (SO-KEY-NODE T))
    (PRINT-OBJECT (TOKEN-BUF T))
    (PRINT-OBJECT (READTABLE T))
    (PRINT-OBJECT (SYMBOL T))
    (PRINT-OBJECT (CONS T))
    (PRINT-OBJECT (VECTOR T))
    (PRINT-OBJECT (ARRAY T))
    (PRINT-OBJECT (INTEGER T))
    (PRINT-OBJECT (RATIO T))
    (PRINT-OBJECT (COMPLEX T))
    (PRINT-OBJECT (FLOAT T))
    (PRINT-OBJECT (CHARACTER T))
    (PRINT-OBJECT (SYSTEM-AREA-POINTER T))
    (PRINT-OBJECT (WEAK-POINTER T))
    (PRINT-OBJECT (CODE-COMPONENT T))
    (PRINT-OBJECT (FDEFN T))
    (PRINT-OBJECT (SIMD-PACK T))
    (PRINT-OBJECT (SIMD-PACK-256 T))
    (PRINT-OBJECT (FUNCTION T))
    (PRINT-OBJECT (T T))
    (PRINT-OBJECT (PPRINT-DISPATCH-ENTRY T))
    (PRINT-OBJECT (PPRINT-DISPATCH-TABLE T))
    (PRINT-OBJECT (STRUCTURE-OBJECT T))
    (PRINT-OBJECT (LOGICAL-HOST T))
    (PRINT-OBJECT (PATHNAME T))
    (PRINT-OBJECT (PATTERN T))
    (PRINT-OBJECT (AVLNODE T))
    (PRINT-OBJECT (THREAD T))
    (PRINT-OBJECT (MUTEX T))
    (PRINT-OBJECT (WAITQUEUE T))
    (PRINT-OBJECT (DEBUG-VAR T))
    (PRINT-OBJECT (DEBUG-FUN T))
    (PRINT-OBJECT (DEBUG-BLOCK T))
    (PRINT-OBJECT (COMPILED-FRAME T))
    (PRINT-OBJECT (BREAKPOINT-DATA T))
    (PRINT-OBJECT (BREAKPOINT T))
    (PRINT-OBJECT (CODE-LOCATION T))
    (PRINT-OBJECT (FUN-END-COOKIE T))
    (PRINT-OBJECT (INSTRUCTION T))
    (PRINT-OBJECT (INST-SPACE T))
    (PRINT-OBJECT (SEGMENT T))
    (PRINT-OBJECT (REG T))
    (PRINT-OBJECT (UNPRINTABLE-OBJECT T))
    (PRINT-OBJECT (LOOP-UNIVERSE T))
    (PRINT-OBJECT (FMT-CONTROL T))
    (PRINT-OBJECT (FORMAT-DIRECTIVE T))
    (PRINT-OBJECT (TRACE-INFO T))
  Source file: SYS:SRC;PCL;PRINT-OBJECT.LISP

```

### `REINITIALIZE-INSTANCE`

```lisp
COMMON-LISP:REINITIALIZE-INSTANCE
  [symbol]

REINITIALIZE-INSTANCE names a generic function:
  Lambda-list: (INSTANCE &REST INITARGS &KEY &ALLOW-OTHER-KEYS)
  Derived type: (FUNCTION
                 (T &REST T &KEY (:DIRECT-SUPERCLASSES T)
                  (:LAMBDA-LIST T) (:ARGUMENT-PRECEDENCE-ORDER T)
                  &ALLOW-OTHER-KEYS)
                 *)
  Method-combination: STANDARD
  Methods:
    (REINITIALIZE-INSTANCE (SLOT-OBJECT))
    (REINITIALIZE-INSTANCE :BEFORE (SLOT-CLASS))
    (REINITIALIZE-INSTANCE :AFTER (SLOT-CLASS))
    (REINITIALIZE-INSTANCE :AFTER (CONDITION-CLASS))
    (REINITIALIZE-INSTANCE (SYSTEM-CLASS))
    (REINITIALIZE-INSTANCE (METHOD))
    (REINITIALIZE-INSTANCE :AROUND (STANDARD-GENERIC-FUNCTION))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `REMOVE-METHOD`

```lisp
COMMON-LISP:REMOVE-METHOD
  [symbol]

REMOVE-METHOD names a generic function:
  Lambda-list: (GENERIC-FUNCTION METHOD)
  Argument precedence order: (GENERIC-FUNCTION METHOD)
  Derived type: (FUNCTION (T T) *)
  Method-combination: STANDARD
  Methods:
    (REMOVE-METHOD (STANDARD-GENERIC-FUNCTION METHOD))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `SHARED-INITIALIZE`

```lisp
COMMON-LISP:SHARED-INITIALIZE
  [symbol]

SHARED-INITIALIZE names a generic function:
  Lambda-list: (INSTANCE SLOT-NAMES &REST INITARGS &KEY
                &ALLOW-OTHER-KEYS)
  Argument precedence order: (INSTANCE SLOT-NAMES)
  Derived type: (FUNCTION
                 (T T &REST T &KEY (:PROTOCOL T) (:TYPE T)
                  (:DIRECT-SUPERCLASSES T) (:DIRECT-SLOTS T)
                  (:DIRECT-DEFAULT-INITARGS T) (:NAME T)
                  (:ALLOCATION T) (:ALLOCATION-CLASS T) (:QUALIFIERS T)
                  (:LAMBDA-LIST T) (:SPECIALIZERS T) (:FUNCTION T)
                  (:DOCUMENTATION T) (:SLOT-NAME T)
                  (:SLOT-DEFINITION T) (SB-PCL::METHOD-CELL T)
                  (:ARGUMENT-PRECEDENCE-ORDER T) (:DECLARATIONS T)
                  (:METHOD-CLASS T) (:METHOD-COMBINATION T)
                  &ALLOW-OTHER-KEYS)
                 *)
  Method-combination: STANDARD
  Methods:
    (SHARED-INITIALIZE :AFTER (SOCKET T))
    (SHARED-INITIALIZE (SLOT-OBJECT T))
    (SHARED-INITIALIZE :AFTER (CLASS-EQ-SPECIALIZER T))
    (SHARED-INITIALIZE :AFTER (EQL-SPECIALIZER T))
    (SHARED-INITIALIZE :AFTER (STD-CLASS T))
    (SHARED-INITIALIZE :AFTER (FORWARD-REFERENCED-CLASS T))
    (SHARED-INITIALIZE :BEFORE (CLASS T))
    (SHARED-INITIALIZE :AFTER (CONDITION-CLASS T))
    (SHARED-INITIALIZE :AFTER (STRUCTURE-SLOT-DEFINITION T))
    (SHARED-INITIALIZE :AFTER (STRUCTURE-CLASS T))
    (SHARED-INITIALIZE :BEFORE (STANDARD-METHOD T))
    (SHARED-INITIALIZE :BEFORE (STANDARD-ACCESSOR-METHOD T))
    (SHARED-INITIALIZE :AFTER (STANDARD-METHOD T))
    (SHARED-INITIALIZE :BEFORE (STANDARD-GENERIC-FUNCTION T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `SLOT-MISSING`

```lisp
COMMON-LISP:SLOT-MISSING
  [symbol]

SLOT-MISSING names a generic function:
  Lambda-list: (CLASS INSTANCE SLOT-NAME OPERATION &OPTIONAL NEW-VALUE)
  Argument precedence order: (CLASS INSTANCE SLOT-NAME OPERATION)
  Derived type: (FUNCTION (T T T T &OPTIONAL T) *)
  Method-combination: STANDARD
  Methods:
    (SLOT-MISSING (FLUID-CLASS FLUID-OBJECT T (EQL SETF)))
      Documentation:
        On attempting to set a missing slot, add the slot to the class,
        then repeat SETF.
    (SLOT-MISSING (FLUID-CLASS FLUID-OBJECT T (EQL SLOT-VALUE)))
      Documentation:
        On attempting to get the value of a missing slot, raise a
        slot-unbound error.
    (SLOT-MISSING (FLUID-CLASS FLUID-OBJECT T (EQL SLOT-MAKUNBOUND)))
      Documentation:
        A missing slot in a fluid class is considered unbound.
    (SLOT-MISSING (FLUID-CLASS FLUID-OBJECT T (EQL SLOT-BOUNDP)))
      Documentation:
        A missing slot in a fluid class is considered unbound.
    (SLOT-MISSING (T T T T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `SLOT-UNBOUND`

```lisp
COMMON-LISP:SLOT-UNBOUND
  [symbol]

SLOT-UNBOUND names a generic function:
  Lambda-list: (CLASS INSTANCE SLOT-NAME)
  Argument precedence order: (CLASS INSTANCE SLOT-NAME)
  Derived type: (FUNCTION (T T T) *)
  Method-combination: STANDARD
  Methods:
    (SLOT-UNBOUND (T DIST (EQL SYSTEM-INDEX)))
    (SLOT-UNBOUND (T RELEASE (EQL PROVIDED-SYSTEMS)))
    (SLOT-UNBOUND (T DIST (EQL RELEASE-INDEX)))
    (SLOT-UNBOUND (T DIST (EQL BASE-DIRECTORY)))
    (SLOT-UNBOUND (T DIST (EQL PROVIDED-RELEASES)))
    (SLOT-UNBOUND (T DIST (EQL PROVIDED-SYSTEMS)))
    (SLOT-UNBOUND (T DIST (EQL AVAILABLE-VERSIONS-URL)))
    (SLOT-UNBOUND (T T T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `STREAM-ELEMENT-TYPE`

```lisp
COMMON-LISP:STREAM-ELEMENT-TYPE
  [symbol]

STREAM-ELEMENT-TYPE names a generic function:
  Lambda-list: (STREAM)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return a type specifier for the kind of object returned by the
      STREAM. The class FUNDAMENTAL-CHARACTER-STREAM provides a default method
      which returns CHARACTER.
  Method-combination: STANDARD
  Methods:
    (STREAM-ELEMENT-TYPE (IO-STREAM))
    (STREAM-ELEMENT-TYPE (FLEXI-STREAM))
      Documentation:
        Returns the element type that was provided by the creator of
        the stream.
    (STREAM-ELEMENT-TYPE (IN-MEMORY-STREAM))
      Documentation:
        The element type is always OCTET by definition.
    (STREAM-ELEMENT-TYPE (FUNDAMENTAL-CHARACTER-STREAM))
    (STREAM-ELEMENT-TYPE (ANSI-STREAM))
  Source file: SYS:SRC;PCL;GRAY-STREAMS.LISP

```

### `UPDATE-INSTANCE-FOR-DIFFERENT-CLASS`

```lisp
COMMON-LISP:UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
  [symbol]

UPDATE-INSTANCE-FOR-DIFFERENT-CLASS names a generic function:
  Lambda-list: (PREVIOUS CURRENT &REST INITARGS)
  Argument precedence order: (PREVIOUS CURRENT)
  Derived type: (FUNCTION (T T &REST T) *)
  Method-combination: STANDARD
  Methods:
    (UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (STANDARD-OBJECT
                                          STANDARD-OBJECT))
    (UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (T METHOD))
    (UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (METHOD T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```

### `UPDATE-INSTANCE-FOR-REDEFINED-CLASS`

```lisp
COMMON-LISP:UPDATE-INSTANCE-FOR-REDEFINED-CLASS
  [symbol]

UPDATE-INSTANCE-FOR-REDEFINED-CLASS names a generic function:
  Lambda-list: (INSTANCE ADDED-SLOTS DISCARDED-SLOTS PROPERTY-LIST
                &REST INITARGS)
  Argument precedence order: (INSTANCE ADDED-SLOTS DISCARDED-SLOTS
                              PROPERTY-LIST)
  Derived type: (FUNCTION (T T T T &REST T) *)
  Method-combination: STANDARD
  Methods:
    (UPDATE-INSTANCE-FOR-REDEFINED-CLASS (STANDARD-OBJECT T T T))
    (UPDATE-INSTANCE-FOR-REDEFINED-CLASS (METHOD T T T))
  Source file: SYS:SRC;PCL;GENERIC-FUNCTIONS.LISP

```


## macro

### Symbol list

1. [AND](#and)
2. [ASSERT](#assert)
3. [CALL-METHOD](#call-method)
4. [CASE](#case)
5. [CCASE](#ccase)
6. [CHECK-TYPE](#check-type)
7. [COND](#cond)
8. [CTYPECASE](#ctypecase)
9. [DECF](#decf)
10. [DECLAIM](#declaim)
11. [DEFCLASS](#defclass)
12. [DEFCONSTANT](#defconstant)
13. [DEFGENERIC](#defgeneric)
14. [DEFINE-COMPILER-MACRO](#define-compiler-macro)
15. [DEFINE-CONDITION](#define-condition)
16. [DEFINE-METHOD-COMBINATION](#define-method-combination)
17. [DEFINE-MODIFY-MACRO](#define-modify-macro)
18. [DEFINE-SETF-EXPANDER](#define-setf-expander)
19. [DEFINE-SYMBOL-MACRO](#define-symbol-macro)
20. [DEFMACRO](#defmacro)
21. [DEFMETHOD](#defmethod)
22. [DEFPACKAGE](#defpackage)
23. [DEFPARAMETER](#defparameter)
24. [DEFSETF](#defsetf)
25. [DEFSTRUCT](#defstruct)
26. [DEFTYPE](#deftype)
27. [DEFUN](#defun)
28. [DEFVAR](#defvar)
29. [DESTRUCTURING-BIND](#destructuring-bind)
30. [DO](#do)
31. [DO*](#do*)
32. [DO-ALL-SYMBOLS](#do-all-symbols)
33. [DO-EXTERNAL-SYMBOLS](#do-external-symbols)
34. [DO-SYMBOLS](#do-symbols)
35. [DOLIST](#dolist)
36. [DOTIMES](#dotimes)
37. [ECASE](#ecase)
38. [ETYPECASE](#etypecase)
39. [FORMATTER](#formatter)
40. [HANDLER-BIND](#handler-bind)
41. [HANDLER-CASE](#handler-case)
42. [IGNORE-ERRORS](#ignore-errors)
43. [IN-PACKAGE](#in-package)
44. [INCF](#incf)
45. [LAMBDA](#lambda)
46. [LOOP](#loop)
47. [LOOP-FINISH](#loop-finish)
48. [MULTIPLE-VALUE-BIND](#multiple-value-bind)
49. [MULTIPLE-VALUE-LIST](#multiple-value-list)
50. [MULTIPLE-VALUE-SETQ](#multiple-value-setq)
51. [NTH-VALUE](#nth-value)
52. [OR](#or)
53. [POP](#pop)
54. [PPRINT-EXIT-IF-LIST-EXHAUSTED](#pprint-exit-if-list-exhausted)
55. [PPRINT-LOGICAL-BLOCK](#pprint-logical-block)
56. [PPRINT-POP](#pprint-pop)
57. [PRINT-UNREADABLE-OBJECT](#print-unreadable-object)
58. [PROG](#prog)
59. [PROG*](#prog*)
60. [PROG1](#prog1)
61. [PROG2](#prog2)
62. [PSETF](#psetf)
63. [PSETQ](#psetq)
64. [PUSH](#push)
65. [PUSHNEW](#pushnew)
66. [REMF](#remf)
67. [RESTART-BIND](#restart-bind)
68. [RESTART-CASE](#restart-case)
69. [RETURN](#return)
70. [ROTATEF](#rotatef)
71. [SETF](#setf)
72. [SHIFTF](#shiftf)
73. [STEP](#step)
74. [TIME](#time)
75. [TRACE](#trace)
76. [TYPECASE](#typecase)
77. [UNLESS](#unless)
78. [UNTRACE](#untrace)
79. [WHEN](#when)
80. [WITH-ACCESSORS](#with-accessors)
81. [WITH-COMPILATION-UNIT](#with-compilation-unit)
82. [WITH-CONDITION-RESTARTS](#with-condition-restarts)
83. [WITH-HASH-TABLE-ITERATOR](#with-hash-table-iterator)
84. [WITH-INPUT-FROM-STRING](#with-input-from-string)
85. [WITH-OPEN-FILE](#with-open-file)
86. [WITH-OPEN-STREAM](#with-open-stream)
87. [WITH-OUTPUT-TO-STRING](#with-output-to-string)
88. [WITH-PACKAGE-ITERATOR](#with-package-iterator)
89. [WITH-SIMPLE-RESTART](#with-simple-restart)
90. [WITH-SLOTS](#with-slots)
91. [WITH-STANDARD-IO-SYNTAX](#with-standard-io-syntax)


### `AND`

```lisp
COMMON-LISP:AND
  [symbol]

AND names a macro:
  Lambda-list: (&REST FORMS)
  Source file: SYS:SRC;CODE;MACROS.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION AND(..

```

### `ASSERT`

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

### `CALL-METHOD`

```lisp
COMMON-LISP:CALL-METHOD
  [symbol]

CALL-METHOD names a macro:
  Lambda-list: (&REST ARGS)
  Source file: SYS:SRC;PCL;COMBIN.LISP

```

### `CASE`

```lisp
COMMON-LISP:CASE
  [symbol]

CASE names a macro:
  Lambda-list: (KEYFORM &BODY CASES)
  Documentation:
    CASE Keyform {({(Key*) | Key} Form*)}*
      Evaluates the Forms in the first clause with a Key EQL to the value of
      Keyform. If a singleton key is T then the clause is a default clause.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `CCASE`

```lisp
COMMON-LISP:CCASE
  [symbol]

CCASE names a macro:
  Lambda-list: (KEYFORM &BODY CASES)
  Documentation:
    CCASE Keyform {({(Key*) | Key} Form*)}*
      Evaluates the Forms in the first clause with a Key EQL to the value of
      Keyform. If none of the keys matches then a correctable error is
      signalled.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `CHECK-TYPE`

```lisp
COMMON-LISP:CHECK-TYPE
  [symbol]

CHECK-TYPE names a macro:
  Lambda-list: (PLACE TYPE &OPTIONAL TYPE-STRING)
  Documentation:
    Signal a restartable error of type TYPE-ERROR if the value of PLACE
    is not of the specified type. If an error is signalled and the restart
    is used to return, this can only return if the STORE-VALUE restart is
    invoked. In that case it will store into PLACE and start over.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `COND`

```lisp
COMMON-LISP:COND
  [symbol]

COND names a macro:
  Lambda-list: (&REST CLAUSES)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `CTYPECASE`

```lisp
COMMON-LISP:CTYPECASE
  [symbol]

CTYPECASE names a macro:
  Lambda-list: (KEYFORM &BODY CASES)
  Documentation:
    CTYPECASE Keyform {(Type Form*)}*
      Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
      is true. If no form is satisfied then a correctable error is signalled.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DECF`

```lisp
COMMON-LISP:DECF
  [symbol]

DECF names a macro:
  Lambda-list: (PLACE &OPTIONAL (DELTA 1))
  Documentation:
    The first argument is some location holding a number. This number is
      decremented by the second argument, DELTA, which defaults to 1.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `DECLAIM`

```lisp
COMMON-LISP:DECLAIM
  [symbol]

DECLAIM names a macro:
  Lambda-list: (&REST SPECS)
  Documentation:
    DECLAIM Declaration*
      Do a declaration or declarations for the global environment.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DEFCLASS`

```lisp
COMMON-LISP:DEFCLASS
  [symbol]

DEFCLASS names a macro:
  Lambda-list: (NAME DIRECT-SUPERCLASSES DIRECT-SLOTS &REST OPTIONS)
  Source file: SYS:SRC;PCL;DEFCLASS.LISP

```

### `DEFCONSTANT`

```lisp
COMMON-LISP:DEFCONSTANT
  [symbol]

DEFCONSTANT names a macro:
  Lambda-list: (NAME VALUE &OPTIONAL (DOC NIL))
  Documentation:
    Define a global constant, saying that the value is constant and may be
      compiled into code. If the variable already has a value, and this is not
      EQL to the new value, the code is not portable (undefined behavior). The
      third argument is an optional documentation string for the variable.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DEFGENERIC`

```lisp
COMMON-LISP:DEFGENERIC
  [symbol]

DEFGENERIC names a macro:
  Lambda-list: (FUN-NAME LAMBDA-LIST &BODY OPTIONS)
  Source file: SYS:SRC;PCL;BOOT.LISP

```

### `DEFINE-COMPILER-MACRO`

```lisp
COMMON-LISP:DEFINE-COMPILER-MACRO
  [symbol]

DEFINE-COMPILER-MACRO names a macro:
  Lambda-list: (NAME LAMBDA-LIST &BODY BODY)
  Documentation:
    Define a compiler-macro for NAME.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DEFINE-CONDITION`

```lisp
COMMON-LISP:DEFINE-CONDITION
  [symbol]

DEFINE-CONDITION names a macro:
  Lambda-list: (NAME (&REST PARENT-TYPES) (&REST SLOT-SPECS) &BODY
                OPTIONS)
  Documentation:
    DEFINE-CONDITION Name (Parent-Type*) (Slot-Spec*) Option*
       Define NAME as a condition type. This new type inherits slots and its
       report function from the specified PARENT-TYPEs. A slot spec is a list of:
         (slot-name :reader <rname> :initarg <iname> {Option Value}*
    
       The DEFINE-CLASS slot options :ALLOCATION, :INITFORM, [slot] :DOCUMENTATION
       and :TYPE and the overall options :DEFAULT-INITARGS and
       [type] :DOCUMENTATION are also allowed.
    
       The :REPORT option is peculiar to DEFINE-CONDITION. Its argument is either
       a string or a two-argument lambda or function name. If a function, the
       function is called with the condition and stream to report the condition.
       If a string, the string is printed.
    
       Condition types are classes, but (as allowed by ANSI and not as described in
       CLtL2) are neither STANDARD-OBJECTs nor STRUCTURE-OBJECTs. WITH-SLOTS and
       SLOT-VALUE may not be used on condition objects.
  Source file: SYS:SRC;CODE;TARGET-ERROR.LISP

```

### `DEFINE-METHOD-COMBINATION`

```lisp
COMMON-LISP:DEFINE-METHOD-COMBINATION
  [symbol]

DEFINE-METHOD-COMBINATION names a macro:
  Lambda-list: (NAME . ARGS)
  Source file: SYS:SRC;PCL;DEFCOMBIN.LISP

```

### `DEFINE-MODIFY-MACRO`

```lisp
COMMON-LISP:DEFINE-MODIFY-MACRO
  [symbol]

DEFINE-MODIFY-MACRO names a macro:
  Lambda-list: (NAME LAMBDA-LIST FUNCTION &OPTIONAL DOC-STRING)
  Documentation:
    Creates a new read-modify-write macro like PUSH or INCF.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `DEFINE-SETF-EXPANDER`

```lisp
COMMON-LISP:DEFINE-SETF-EXPANDER
  [symbol]

DEFINE-SETF-EXPANDER names a macro:
  Lambda-list: (ACCESS-FN LAMBDA-LIST &BODY BODY)
  Documentation:
    Syntax like DEFMACRO, but creates a setf expander function. The body
      of the definition must be a form that returns five appropriate values.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `DEFINE-SYMBOL-MACRO`

```lisp
COMMON-LISP:DEFINE-SYMBOL-MACRO
  [symbol]

DEFINE-SYMBOL-MACRO names a macro:
  Lambda-list: (NAME EXPANSION)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DEFMACRO`

```lisp
COMMON-LISP:DEFMACRO
  [symbol]

DEFMACRO names a macro:
  Lambda-list: (NAME LAMBDA-LIST &BODY BODY)
  Source file: SYS:SRC;CODE;DEFMACRO.LISP

```

### `DEFMETHOD`

```lisp
COMMON-LISP:DEFMETHOD
  [symbol]

DEFMETHOD names a macro:
  Lambda-list: (NAME &REST ARGS)
  Source file: SYS:SRC;PCL;BOOT.LISP

```

### `DEFPACKAGE`

```lisp
COMMON-LISP:DEFPACKAGE
  [symbol]

DEFPACKAGE names a macro:
  Lambda-list: (PACKAGE &REST OPTIONS)
  Documentation:
    Defines a new package called PACKAGE. Each of OPTIONS should be one of the
       following: 
        (USE {package-name}*)
        (EXPORT {symbol-name}*)
        (IMPORT-FROM <package-name> {symbol-name}*)
        (SHADOW {symbol-name}*)
        (SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
        (LOCAL-NICKNAMES {(local-nickname actual-package-name)}*)
        (LOCK boolean)
        (IMPLEMENT {package-name}*)
        (DOCUMENTATION doc-string)
        (INTERN {symbol-name}*)
        (SIZE <integer>)
        (NICKNAMES {package-name}*)
       All options except SIZE, LOCK, and :DOCUMENTATION can be used multiple
       times.
  Source file: SYS:SRC;CODE;DEFPACKAGE.LISP

```

### `DEFPARAMETER`

```lisp
COMMON-LISP:DEFPARAMETER
  [symbol]

DEFPARAMETER names a macro:
  Lambda-list: (VAR VAL &OPTIONAL (DOC NIL))
  Documentation:
    Define a parameter that is not normally changed by the program,
      but that may be changed without causing an error. Declare the
      variable special and sets its value to VAL, overwriting any
      previous value. The third argument is an optional documentation
      string for the parameter.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DEFSETF`

```lisp
COMMON-LISP:DEFSETF
  [symbol]

DEFSETF names a macro:
  Lambda-list: (ACCESS-FN &REST REST)
  Documentation:
    Associates a SETF update function or macro with the specified access
      function or macro. The format is complex. See the manual for details.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `DEFSTRUCT`

```lisp
COMMON-LISP:DEFSTRUCT
  [symbol]

DEFSTRUCT names a macro:
  Lambda-list: (NAME-AND-OPTIONS &REST SLOT-DESCRIPTIONS)
  Documentation:
    DEFSTRUCT {Name | (Name Option*)} [Documentation] {Slot | (Slot [Default] {Key Value}*)}
       Define the structure type Name. Instances are created by MAKE-<name>,
       which takes &KEY arguments allowing initial slot values to the specified.
       A SETF'able function <name>-<slot> is defined for each slot to read and
       write slot values. <name>-p is a type predicate.
    
       Popular DEFSTRUCT options (see manual for others):
    
       (:CONSTRUCTOR Name)
       (:PREDICATE Name)
           Specify the name for the constructor or predicate.
    
       (:CONSTRUCTOR Name Lambda-List)
           Specify the name and arguments for a BOA constructor
           (which is more efficient when keyword syntax isn't necessary.)
    
       (:INCLUDE Supertype Slot-Spec*)
           Make this type a subtype of the structure type Supertype. The optional
           Slot-Specs override inherited slot options.
    
       Slot options:
    
       :TYPE Type-Spec
           Asserts that the value of this slot is always of the specified type.
    
       :READ-ONLY {T | NIL}
           If true, no setter function is defined for this slot.
  Source file: SYS:SRC;CODE;DEFSTRUCT.LISP

```

### `DEFTYPE`

```lisp
COMMON-LISP:DEFTYPE
  [symbol]

DEFTYPE names a macro:
  Lambda-list: (NAME LAMBDA-LIST &BODY BODY)
  Documentation:
    Define a new type, with syntax like DEFMACRO.
  Source file: SYS:SRC;CODE;DEFTYPE.LISP

```

### `DEFUN`

```lisp
COMMON-LISP:DEFUN
  [symbol]

DEFUN names a macro:
  Lambda-list: (NAME LAMBDA-LIST &BODY BODY)
  Documentation:
    Define a function at top level.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DEFVAR`

```lisp
COMMON-LISP:DEFVAR
  [symbol]

DEFVAR names a macro:
  Lambda-list: (VAR &OPTIONAL (VAL NIL) (DOC NIL))
  Documentation:
    Define a special variable at top level. Declare the variable
      SPECIAL and, optionally, initialize it. If the variable already has a
      value, the old value is not clobbered. The third argument is an optional
      documentation string for the variable.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DESTRUCTURING-BIND`

```lisp
COMMON-LISP:DESTRUCTURING-BIND
  [symbol]

DESTRUCTURING-BIND names a macro:
  Lambda-list: (LAMBDA-LIST EXPRESSION &BODY BODY)
  Documentation:
    Bind the variables in LAMBDA-LIST to the corresponding values in the
    tree structure resulting from the evaluation of EXPRESSION.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DO`

```lisp
COMMON-LISP:DO
  [symbol]

DO names a macro:
  Lambda-list: (VARLIST ENDLIST &BODY BODY)
  Documentation:
    DO ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
      Iteration construct. Each Var is initialized in parallel to the value of the
      specified Init form. On subsequent iterations, the Vars are assigned the
      value of the Step form (if any) in parallel. The Test is evaluated before
      each evaluation of the body Forms. When the Test is true, the Exit-Forms
      are evaluated as a PROGN, with the result being the value of the DO. A block
      named NIL is established around the entire expansion, allowing RETURN to be
      used as an alternate exit mechanism.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DO*`

```lisp
COMMON-LISP:DO*
  [symbol]

DO* names a macro:
  Lambda-list: (VARLIST ENDLIST &BODY BODY)
  Documentation:
    DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
      Iteration construct. Each Var is initialized sequentially (like LET*) to the
      value of the specified Init form. On subsequent iterations, the Vars are
      sequentially assigned the value of the Step form (if any). The Test is
      evaluated before each evaluation of the body Forms. When the Test is true,
      the Exit-Forms are evaluated as a PROGN, with the result being the value
      of the DO. A block named NIL is established around the entire expansion,
      allowing RETURN to be used as an alternate exit mechanism.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DO-ALL-SYMBOLS`

```lisp
COMMON-LISP:DO-ALL-SYMBOLS
  [symbol]

DO-ALL-SYMBOLS names a macro:
  Lambda-list: ((VAR &OPTIONAL RESULT-FORM) &BODY BODY-DECLS)
  Documentation:
    DO-ALL-SYMBOLS (VAR [RESULT-FORM]) {DECLARATION}* {TAG | FORM}*
       Executes the FORMs once for each symbol in every package with VAR bound
       to the current symbol.
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `DO-EXTERNAL-SYMBOLS`

```lisp
COMMON-LISP:DO-EXTERNAL-SYMBOLS
  [symbol]

DO-EXTERNAL-SYMBOLS names a macro:
  Lambda-list: ((VAR &OPTIONAL (PACKAGE (QUOTE *PACKAGE*)) RESULT-FORM)
                &BODY BODY-DECLS)
  Documentation:
    DO-EXTERNAL-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECL}* {TAG | FORM}*
       Executes the FORMs once for each external symbol in the given PACKAGE with
       VAR bound to the current symbol.
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `DO-SYMBOLS`

```lisp
COMMON-LISP:DO-SYMBOLS
  [symbol]

DO-SYMBOLS names a macro:
  Lambda-list: ((VAR &OPTIONAL (PACKAGE (QUOTE *PACKAGE*)) RESULT-FORM)
                &BODY BODY-DECLS)
  Documentation:
    DO-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECLARATION}* {TAG | FORM}*
       Executes the FORMs at least once for each symbol accessible in the given
       PACKAGE with VAR bound to the current symbol.
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `DOLIST`

```lisp
COMMON-LISP:DOLIST
  [symbol]

DOLIST names a macro:
  Lambda-list: ((VAR LIST &OPTIONAL (RESULT NIL)) &BODY BODY)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `DOTIMES`

```lisp
COMMON-LISP:DOTIMES
  [symbol]

DOTIMES names a macro:
  Lambda-list: ((VAR COUNT &OPTIONAL (RESULT NIL)) &BODY BODY)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `ECASE`

```lisp
COMMON-LISP:ECASE
  [symbol]

ECASE names a macro:
  Lambda-list: (KEYFORM &BODY CASES)
  Documentation:
    ECASE Keyform {({(Key*) | Key} Form*)}*
      Evaluates the Forms in the first clause with a Key EQL to the value of
      Keyform. If none of the keys matches then an error is signalled.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `ETYPECASE`

```lisp
COMMON-LISP:ETYPECASE
  [symbol]

ETYPECASE names a macro:
  Lambda-list: (KEYFORM &BODY CASES)
  Documentation:
    ETYPECASE Keyform {(Type Form*)}*
      Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
      is true. If no form is satisfied then an error is signalled.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `FORMATTER`

```lisp
COMMON-LISP:FORMATTER
  [symbol]

FORMATTER names a macro:
  Lambda-list: (CONTROL-STRING)
  Source file: SYS:SRC;CODE;FORMAT.LISP

```

### `HANDLER-BIND`

```lisp
COMMON-LISP:HANDLER-BIND
  [symbol]

HANDLER-BIND names a macro:
  Lambda-list: (BINDINGS &BODY FORMS)
  Documentation:
    (HANDLER-BIND ( {(type handler)}* ) body)
    
    Executes body in a dynamic context where the given handler bindings are in
    effect. Each handler must take the condition being signalled as an argument.
    The bindings are searched first to last in the event of a signalled
    condition.
  Source file: SYS:SRC;CODE;ERROR.LISP

```

### `HANDLER-CASE`

```lisp
COMMON-LISP:HANDLER-CASE
  [symbol]

HANDLER-CASE names a macro:
  Lambda-list: (FORM &REST CASES)
  Documentation:
    (HANDLER-CASE form { (type ([var]) body) }* )
    
    Execute FORM in a context with handlers established for the condition types. A
    peculiar property allows type to be :NO-ERROR. If such a clause occurs, and
    form returns normally, all its values are passed to this clause as if by
    MULTIPLE-VALUE-CALL. The :NO-ERROR clause accepts more than one var
    specification.
  Source file: SYS:SRC;CODE;ERROR.LISP

```

### `IGNORE-ERRORS`

```lisp
COMMON-LISP:IGNORE-ERRORS
  [symbol]

IGNORE-ERRORS names a macro:
  Lambda-list: (&REST FORMS)
  Documentation:
    Execute FORMS handling ERROR conditions, returning the result of the last
      form, or (VALUES NIL the-ERROR-that-was-caught) if an ERROR was handled.
  Source file: SYS:SRC;CODE;ERROR.LISP

```

### `IN-PACKAGE`

```lisp
COMMON-LISP:IN-PACKAGE
  [symbol]

IN-PACKAGE names a macro:
  Lambda-list: (STRING-DESIGNATOR)
  Source file: SYS:SRC;CODE;PACKAGE.LISP

```

### `INCF`

```lisp
COMMON-LISP:INCF
  [symbol]

INCF names a macro:
  Lambda-list: (PLACE &OPTIONAL (DELTA 1))
  Documentation:
    The first argument is some location holding a number. This number is
      incremented by the second argument, DELTA, which defaults to 1.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `LAMBDA`

```lisp
COMMON-LISP:LAMBDA
  [symbol]

LAMBDA names a macro:
  Lambda-list: (ARGS &BODY BODY)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `LOOP`

```lisp
COMMON-LISP:LOOP
  [symbol]

LOOP names a macro:
  Lambda-list: (&REST KEYWORDS-AND-FORMS)
  Source file: SYS:SRC;CODE;LOOP.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION LOOP..

```

### `LOOP-FINISH`

```lisp
COMMON-LISP:LOOP-FINISH
  [symbol]

LOOP-FINISH names a macro:
  Lambda-list: ()
  Documentation:
    Cause the iteration to terminate "normally", the same as implicit
    termination by an iteration driving clause, or by use of WHILE or
    UNTIL -- the epilogue code (if any) will be run, and any implicitly
    collected result will be returned as the value of the LOOP.
  Source file: SYS:SRC;CODE;LOOP.LISP

```

### `MULTIPLE-VALUE-BIND`

```lisp
COMMON-LISP:MULTIPLE-VALUE-BIND
  [symbol]

MULTIPLE-VALUE-BIND names a macro:
  Lambda-list: (VARS VALUE-FORM &BODY BODY)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `MULTIPLE-VALUE-LIST`

```lisp
COMMON-LISP:MULTIPLE-VALUE-LIST
  [symbol]

MULTIPLE-VALUE-LIST names a macro:
  Lambda-list: (VALUE-FORM)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `MULTIPLE-VALUE-SETQ`

```lisp
COMMON-LISP:MULTIPLE-VALUE-SETQ
  [symbol]

MULTIPLE-VALUE-SETQ names a macro:
  Lambda-list: (VARS VALUE-FORM)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `NTH-VALUE`

```lisp
COMMON-LISP:NTH-VALUE
  [symbol]

NTH-VALUE names a macro:
  Lambda-list: (N FORM)
  Documentation:
    Evaluate FORM and return the Nth value (zero based)
     without consing a temporary list of values.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `OR`

```lisp
COMMON-LISP:OR
  [symbol]

OR names a macro:
  Lambda-list: (&REST FORMS)
  Source file: SYS:SRC;CODE;MACROS.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION OR(R..

```

### `POP`

```lisp
COMMON-LISP:POP
  [symbol]

POP names a macro:
  Lambda-list: (PLACE)
  Documentation:
    The argument is a location holding a list. Pops one item off the front
      of the list and returns it.
  Source file: SYS:SRC;CODE;SETF.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION POP(..

```

### `PPRINT-EXIT-IF-LIST-EXHAUSTED`

```lisp
COMMON-LISP:PPRINT-EXIT-IF-LIST-EXHAUSTED
  [symbol]

PPRINT-EXIT-IF-LIST-EXHAUSTED names a macro:
  Lambda-list: ()
  Documentation:
    Cause the closest enclosing use of PPRINT-LOGICAL-BLOCK to return
       if its list argument is exhausted. Can only be used inside
       PPRINT-LOGICAL-BLOCK, and only when the LIST argument to
       PPRINT-LOGICAL-BLOCK is supplied.
  Source file: SYS:SRC;CODE;EARLY-PPRINT.LISP

```

### `PPRINT-LOGICAL-BLOCK`

```lisp
COMMON-LISP:PPRINT-LOGICAL-BLOCK
  [symbol]

PPRINT-LOGICAL-BLOCK names a macro:
  Lambda-list: ((STREAM-SYMBOL OBJECT &REST KEYS &KEY (PREFIX NIL)
                 (PER-LINE-PREFIX NIL) (SUFFIX ))
                &BODY BODY)
  Documentation:
    Group some output into a logical block. STREAM-SYMBOL should be either a
       stream, T (for *TERMINAL-IO*), or NIL (for *STANDARD-OUTPUT*). The printer
       control variable *PRINT-LEVEL* is automatically handled.
  Source file: SYS:SRC;CODE;EARLY-PPRINT.LISP

```

### `PPRINT-POP`

```lisp
COMMON-LISP:PPRINT-POP
  [symbol]

PPRINT-POP names a macro:
  Lambda-list: ()
  Documentation:
    Return the next element from LIST argument to the closest enclosing
       use of PPRINT-LOGICAL-BLOCK, automatically handling *PRINT-LENGTH*
       and *PRINT-CIRCLE*. Can only be used inside PPRINT-LOGICAL-BLOCK.
       If the LIST argument to PPRINT-LOGICAL-BLOCK was NIL, then nothing
       is popped, but the *PRINT-LENGTH* testing still happens.
  Source file: SYS:SRC;CODE;EARLY-PPRINT.LISP

```

### `PRINT-UNREADABLE-OBJECT`

```lisp
COMMON-LISP:PRINT-UNREADABLE-OBJECT
  [symbol]

PRINT-UNREADABLE-OBJECT names a macro:
  Lambda-list: ((OBJECT STREAM &KEY TYPE IDENTITY) &BODY BODY)
  Documentation:
    Output OBJECT to STREAM with "#<" prefix, ">" suffix, optionally
      with object-type prefix and object-identity suffix, and executing the
      code in BODY to provide possible further output.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `PROG`

```lisp
COMMON-LISP:PROG
  [symbol]

PROG names a macro:
  Lambda-list: (VARLIST &BODY BODY-DECLS)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `PROG*`

```lisp
COMMON-LISP:PROG*
  [symbol]

PROG* names a macro:
  Lambda-list: (VARLIST &BODY BODY-DECLS)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `PROG1`

```lisp
COMMON-LISP:PROG1
  [symbol]

PROG1 names a macro:
  Lambda-list: (RESULT &BODY BODY)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `PROG2`

```lisp
COMMON-LISP:PROG2
  [symbol]

PROG2 names a macro:
  Lambda-list: (FORM1 RESULT &BODY BODY)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `PSETF`

```lisp
COMMON-LISP:PSETF
  [symbol]

PSETF names a macro:
  Lambda-list: (&REST PAIRS)
  Documentation:
    This is to SETF as PSETQ is to SETQ. Args are alternating place
      expressions and values to go into those places. All of the subforms and
      values are determined, left to right, and only then are the locations
      updated. Returns NIL.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `PSETQ`

```lisp
COMMON-LISP:PSETQ
  [symbol]

PSETQ names a macro:
  Lambda-list: (&REST PAIRS)
  Documentation:
    PSETQ {var value}*
       Set the variables to the values, like SETQ, except that assignments
       happen in parallel, i.e. no assignments take place until all the
       forms have been evaluated.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `PUSH`

```lisp
COMMON-LISP:PUSH
  [symbol]

PUSH names a macro:
  Lambda-list: (OBJ PLACE)
  Documentation:
    Takes an object and a location holding a list. Conses the object onto
      the list, returning the modified list. OBJ is evaluated before PLACE.
  Source file: SYS:SRC;CODE;SETF.LISP

Symbol-plist:
  SB-DISASSEM::INSTRUCTIONS -> (#<SB-DISASSEM:INSTRUCTION PUSH..

```

### `PUSHNEW`

```lisp
COMMON-LISP:PUSHNEW
  [symbol]

PUSHNEW names a macro:
  Lambda-list: (OBJ PLACE &KEY KEY TEST TEST-NOT)
  Documentation:
    Takes an object and a location holding a list. If the object is
      already in the list, does nothing; otherwise, conses the object onto
      the list. Keyword arguments are accepted as per the ADJOIN function.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `REMF`

```lisp
COMMON-LISP:REMF
  [symbol]

REMF names a macro:
  Lambda-list: (PLACE INDICATOR)
  Documentation:
    Place may be any place expression acceptable to SETF, and is expected
      to hold a property list or (). This list is destructively altered to
      remove the property specified by the indicator. Returns T if such a
      property was present, NIL if not.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `RESTART-BIND`

```lisp
COMMON-LISP:RESTART-BIND
  [symbol]

RESTART-BIND names a macro:
  Lambda-list: (BINDINGS &BODY FORMS)
  Documentation:
    (RESTART-BIND ({(case-name function {keyword value}*)}*) forms)
       Executes forms in a dynamic context where the given bindings are in
       effect. Users probably want to use RESTART-CASE. A case-name of NIL
       indicates an anonymous restart. When bindings contain the same
       restart name, FIND-RESTART will find the first such binding.
  Source file: SYS:SRC;CODE;RESTART.LISP

```

### `RESTART-CASE`

```lisp
COMMON-LISP:RESTART-CASE
  [symbol]

RESTART-CASE names a macro:
  Lambda-list: (EXPRESSION &BODY CLAUSES)
  Documentation:
    (RESTART-CASE form {(case-name arg-list {keyword value}* body)}*)
       The form is evaluated in a dynamic context where the clauses have
       special meanings as points to which control may be transferred (see
       INVOKE-RESTART).  When clauses contain the same case-name,
       FIND-RESTART will find the first such clause. If form is a call to
       SIGNAL, ERROR, CERROR or WARN (or macroexpands into such) then the
       signalled condition will be associated with the new restarts.
  Source file: SYS:SRC;CODE;RESTART.LISP

```

### `RETURN`

```lisp
COMMON-LISP:RETURN
  [symbol]

RETURN names a macro:
  Lambda-list: (&OPTIONAL (VALUE NIL))
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `ROTATEF`

```lisp
COMMON-LISP:ROTATEF
  [symbol]

ROTATEF names a macro:
  Lambda-list: (&REST ARGS)
  Documentation:
    Takes any number of SETF-style place expressions. Evaluates all of the
       expressions in turn, then assigns to each place the value of the form to
       its right. The rightmost form gets the value of the leftmost.
       Returns NIL.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `SETF`

```lisp
COMMON-LISP:SETF
  [symbol]

SETF names a macro:
  Lambda-list: (&REST ARGS)
  Documentation:
    Takes pairs of arguments like SETQ. The first is a place and the second
      is the value that is supposed to go into that place. Returns the last
      value. The place argument may be any of the access forms for which SETF
      knows a corresponding setting form.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `SHIFTF`

```lisp
COMMON-LISP:SHIFTF
  [symbol]

SHIFTF names a macro:
  Lambda-list: (&REST ARGS)
  Documentation:
    One or more SETF-style place expressions, followed by a single
       value expression. Evaluates all of the expressions in turn, then
       assigns the value of each expression to the place on its left,
       returning the value of the leftmost.
  Source file: SYS:SRC;CODE;SETF.LISP

```

### `STEP`

```lisp
COMMON-LISP:STEP
  [symbol]

STEP names a macro:
  Lambda-list: (FORM)
  Documentation:
    The form is evaluated with single stepping enabled. Function calls
    outside the lexical scope of the form can be stepped into only if the
    functions in question have been compiled with sufficient DEBUG policy
    to be at least partially steppable.
  Source file: SYS:SRC;CODE;STEP.LISP

```

### `TIME`

```lisp
COMMON-LISP:TIME
  [symbol]

TIME names a macro:
  Lambda-list: (FORM)
  Documentation:
    Execute FORM and print timing information on *TRACE-OUTPUT*.
    
    On some hardware platforms estimated processor cycle counts are
    included in this output; this number is slightly inflated, since it
    includes the pipeline involved in reading the cycle counter --
    executing (TIME NIL) a few times will give you an idea of the
    overhead, and its variance. The cycle counters are also per processor,
    not per thread: if multiple threads are running on the same processor,
    the reported counts will include cycles taken up by all threads
    running on the processor where TIME was executed. Furthermore, if the
    operating system migrates the thread to another processor between
    reads of the cycle counter, the results will be completely bogus.
    Finally, the counter is cycle counter, incremented by the hardware
    even when the process is halted -- which is to say that cycles pass
    normally during operations like SLEEP.
  Source file: SYS:SRC;CODE;TIME.LISP

```

### `TRACE`

```lisp
COMMON-LISP:TRACE
  [symbol]

TRACE names a macro:
  Lambda-list: (&REST SPECS)
  Documentation:
    TRACE {Option Global-Value}* {Name {Option Value}*}*
    
    TRACE is a debugging tool that provides information when specified
    functions are called. In its simplest form:
    
           (TRACE NAME-1 NAME-2 ...)
    
    The NAMEs are not evaluated. Each may be one of the following:
      * SYMBOL, denoting a function or macro.
      * FNAME, a valid function name, denoting a function.
      * (METHOD FNAME QUALIFIERS* (SPECIALIZERS*)) denoting a method.
      * (COMPILER-MACRO SYMBOL) denoting a compiler macro.
      * (LABELS FNAME :IN OUTER-NAME) or (FLET FNAME :IN OUTER-NAME)
        denoting a local function where OUTER-NAME may be any of the
        previous names for functions, macros, methods or compiler macros.
        Tracing local functions may require DEBUG policy 3 to inhibit
        inlining.
      * STRING denoting all functions fbound to symbols whose home package
        is the package with the given name.
    
    Options allow modification of the default behavior. Each option is a
    pair of an option keyword and a value form. Global options are
    specified before the first name, and affect all functions traced by a
    given use of TRACE. Options may also be interspersed with function
    names, in which case they act as local options, only affecting tracing
    of the immediately preceding function name. Local options override
    global options.
    
    By default, TRACE causes a printout on *TRACE-OUTPUT* each time that
    one of the named functions is entered or returns. (This is the basic,
    ANSI Common Lisp behavior of TRACE.)
    
    The following options are defined:
    
       :REPORT Report-Type
           If Report-Type is TRACE (the default) then information is
           reported by printing immediately. If Report-Type is NIL, then
           the only effect of the trace is to execute other
           options (e.g. PRINT or BREAK). Otherwise, Report-Type is
           treated as a function designator and, for each trace event,
           funcalled with 5 arguments: trace depth (a non-negative
           integer), a function name or a function object, a
           keyword (:ENTER, :EXIT or :NON-LOCAL-EXIT), a stack frame, and
           a list of values (arguments or return values).
    
       :CONDITION Form
       :CONDITION-AFTER Form
       :CONDITION-ALL Form
           If :CONDITION is specified, then TRACE does nothing unless Form
           evaluates to true at the time of the call. :CONDITION-AFTER is
           similar, but suppresses the initial printout, and is tested when the
           function returns. :CONDITION-ALL tries both before and after.
    
       :BREAK Form
       :BREAK-AFTER Form
       :BREAK-ALL Form
           If specified, and Form evaluates to true, then the debugger is invoked
           at the start of the function, at the end of the function, or both,
           according to the respective option.
    
       :PRINT Form
       :PRINT-AFTER Form
       :PRINT-ALL Form
           In addition to the usual printout, the result of evaluating Form is
           printed at the start of the function, at the end of the function, or
           both, according to the respective option. Multiple print options cause
           multiple values to be printed.
    
       :WHEREIN Names
           If specified, Names is a function name or list of names. TRACE does
           nothing unless a call to one of those functions encloses the call to
           this function (i.e. it would appear in a backtrace.)  Anonymous
           functions have string names like "DEFUN FOO".
    
       :ENCAPSULATE {:DEFAULT | T | NIL}
           If T, the default, tracing is done via encapsulation (redefining the
           function name) rather than by modifying the function.  :DEFAULT is
           not the default, but means to use encapsulation for interpreted
           functions and funcallable instances, breakpoints otherwise. When
           encapsulation is used, forms are *not* evaluated in the function's
           lexical environment, but SB-DEBUG:ARG can still be used.
    
       :METHODS {T | NIL}
           If T, any function argument naming a generic function will have its
           methods traced in addition to the generic function itself.
    
       :FUNCTION Function-Form
           This is a not really an option, but rather another way of specifying
           what function to trace. The Function-Form is evaluated immediately,
           and the resulting function is traced.
    
    :CONDITION, :BREAK and :PRINT forms are evaluated in a context which
    mocks up the lexical environment of the called function, so that
    SB-DEBUG:VAR and SB-DEBUG:ARG can be used.
    The -AFTER and -ALL forms can use also use SB-DEBUG:ARG. In forms
    which are evaluated after the function call, (SB-DEBUG:ARG N) returns
    the N-th value returned by the function.
  Source file: SYS:SRC;CODE;NTRACE.LISP

```

### `TYPECASE`

```lisp
COMMON-LISP:TYPECASE
  [symbol]

TYPECASE names a macro:
  Lambda-list: (KEYFORM &BODY CASES)
  Documentation:
    TYPECASE Keyform {(Type Form*)}*
      Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
      is true.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `UNLESS`

```lisp
COMMON-LISP:UNLESS
  [symbol]

UNLESS names a macro:
  Lambda-list: (TEST &BODY FORMS)
  Documentation:
    If the first argument is not true, the rest of the forms are
    evaluated as a PROGN.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `UNTRACE`

```lisp
COMMON-LISP:UNTRACE
  [symbol]

UNTRACE names a macro:
  Lambda-list: (&REST SPECS)
  Documentation:
    Remove tracing from the specified functions. Untraces all
    functions when called with no arguments.
  Source file: SYS:SRC;CODE;NTRACE.LISP

```

### `WHEN`

```lisp
COMMON-LISP:WHEN
  [symbol]

WHEN names a macro:
  Lambda-list: (TEST &BODY FORMS)
  Documentation:
    If the first argument is true, the rest of the forms are
    evaluated as a PROGN.
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `WITH-ACCESSORS`

```lisp
COMMON-LISP:WITH-ACCESSORS
  [symbol]

WITH-ACCESSORS names a macro:
  Lambda-list: (SLOTS INSTANCE &BODY BODY)
  Source file: SYS:SRC;PCL;BOOT.LISP

```

### `WITH-COMPILATION-UNIT`

```lisp
COMMON-LISP:WITH-COMPILATION-UNIT
  [symbol]

WITH-COMPILATION-UNIT names a macro:
  Lambda-list: (OPTIONS &BODY BODY)
  Documentation:
    Affects compilations that take place within its dynamic extent. It is
    intended to be eg. wrapped around the compilation of all files in the same system.
    
    Following options are defined:
    
      :OVERRIDE Boolean-Form
          One of the effects of this form is to delay undefined warnings until the
          end of the form, instead of giving them at the end of each compilation.
          If OVERRIDE is NIL (the default), then the outermost
          WITH-COMPILATION-UNIT form grabs the undefined warnings. Specifying
          OVERRIDE true causes that form to grab any enclosed warnings, even if it
          is enclosed by another WITH-COMPILATION-UNIT.
    
      :POLICY Optimize-Declaration-Form
          Provides dynamic scoping for global compiler optimization qualities and
          restrictions, limiting effects of subsequent OPTIMIZE proclamations and
          calls to SB-EXT:RESTRICT-COMPILER-POLICY to the dynamic scope of BODY.
    
          If OVERRIDE is false, specified POLICY is merged with current global
          policy. If OVERRIDE is true, current global policy, including any
          restrictions, is discarded in favor of the specified POLICY.
    
          Supplying POLICY NIL is equivalent to the option not being supplied at
          all, ie. dynamic scoping of policy does not take place.
    
          This option is an SBCL-specific experimental extension: Interface
          subject to change.
    
      :SOURCE-NAMESTRING Namestring-Form
          Attaches the value returned by the Namestring-Form to the internal
          debug-source information as the namestring of the source file. Normally
          the namestring of the input-file for COMPILE-FILE is used: this option
          can be used to provide source-file information for functions compiled
          using COMPILE, or to override the input-file of COMPILE-FILE.
    
          If both an outer and an inner WITH-COMPILATION-UNIT provide a
          SOURCE-NAMESTRING, the inner one takes precedence. Unaffected
          by :OVERRIDE.
    
          This is an SBCL-specific extension.
    
      :SOURCE-PLIST Plist-Form
          Attaches the value returned by the Plist-Form to internal debug-source
          information of functions compiled in within the dynamic extent of BODY.
    
          Primarily for use by development environments, in order to eg. associate
          function definitions with editor-buffers. Can be accessed using
          SB-INTROSPECT:DEFINITION-SOURCE-PLIST.
    
          If an outer WITH-COMPILATION-UNIT form also provide a SOURCE-PLIST, it
          is appended to the end of the provided SOURCE-PLIST. Unaffected
          by :OVERRIDE.
    
          This is an SBCL-specific extension.
    
    Examples:
    
      ;; Prevent proclamations from the file leaking, and restrict
      ;; SAFETY to 3 -- otherwise uses the current global policy.
      (with-compilation-unit (:policy '(optimize))
        (restrict-compiler-policy 'safety 3)
        (load "foo.lisp"))
    
      ;; Using default policy instead of the current global one,
      ;; except for DEBUG 3.
      (with-compilation-unit (:policy '(optimize debug)
                              :override t)
        (load "foo.lisp"))
    
      ;; Same as if :POLICY had not been specified at all: SAFETY 3
      ;; proclamation leaks out from WITH-COMPILATION-UNIT.
      (with-compilation-unit (:policy nil)
        (declaim (optimize safety))
        (load "foo.lisp"))

  Source file: SYS:SRC;COMPILER;MAIN.LISP

```

### `WITH-CONDITION-RESTARTS`

```lisp
COMMON-LISP:WITH-CONDITION-RESTARTS
  [symbol]

WITH-CONDITION-RESTARTS names a macro:
  Lambda-list: (CONDITION-FORM RESTARTS-FORM &BODY BODY)
  Documentation:
    Evaluates the BODY in a dynamic environment where the restarts in the list
       RESTARTS-FORM are associated with the condition returned by CONDITION-FORM.
       This allows FIND-RESTART, etc., to recognize restarts that are not related
       to the error currently being debugged. See also RESTART-CASE.
  Source file: SYS:SRC;CODE;RESTART.LISP

```

### `WITH-HASH-TABLE-ITERATOR`

```lisp
COMMON-LISP:WITH-HASH-TABLE-ITERATOR
  [symbol]

WITH-HASH-TABLE-ITERATOR names a macro:
  Lambda-list: ((NAME HASH-TABLE) &BODY BODY)
  Documentation:
    WITH-HASH-TABLE-ITERATOR ((name hash-table) &body body)
    
    Provides a method of manually looping over the elements of a hash-table. NAME
    is bound to a generator-macro that, within the scope of the invocation,
    returns one or three values. The first value tells whether any objects remain
    in the hash table. When the first value is non-NIL, the second and third
    values are the key and the value of the next object.
    
    Consequences are undefined if HASH-TABLE is mutated during execution of BODY,
    except for changing or removing elements corresponding to the current key. The
    applies to all threads, not just the current one -- even for synchronized
    hash-tables. If the table may be mutated by another thread during iteration,
    use eg. SB-EXT:WITH-LOCKED-HASH-TABLE to protect the WITH-HASH-TABLE-ITERATOR
    for.
  Source file: SYS:SRC;CODE;MAPHASH.LISP

```

### `WITH-INPUT-FROM-STRING`

```lisp
COMMON-LISP:WITH-INPUT-FROM-STRING
  [symbol]

WITH-INPUT-FROM-STRING names a macro:
  Lambda-list: ((VAR STRING &KEY INDEX (START 0) END) &BODY FORMS-DECLS)
  Source file: SYS:SRC;CODE;ANSI-STREAM.LISP

```

### `WITH-OPEN-FILE`

```lisp
COMMON-LISP:WITH-OPEN-FILE
  [symbol]

WITH-OPEN-FILE names a macro:
  Lambda-list: ((STREAM FILESPEC &REST OPTIONS) &BODY BODY)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `WITH-OPEN-STREAM`

```lisp
COMMON-LISP:WITH-OPEN-STREAM
  [symbol]

WITH-OPEN-STREAM names a macro:
  Lambda-list: ((VAR STREAM) &BODY BODY)
  Source file: SYS:SRC;CODE;MACROS.LISP

```

### `WITH-OUTPUT-TO-STRING`

```lisp
COMMON-LISP:WITH-OUTPUT-TO-STRING
  [symbol]

WITH-OUTPUT-TO-STRING names a macro:
  Lambda-list: ((VAR &OPTIONAL STRING &KEY
                 (ELEMENT-TYPE (QUOTE (QUOTE CHARACTER))))
                &BODY BODY)
  Source file: SYS:SRC;CODE;ANSI-STREAM.LISP

```

### `WITH-PACKAGE-ITERATOR`

```lisp
COMMON-LISP:WITH-PACKAGE-ITERATOR
  [symbol]

WITH-PACKAGE-ITERATOR names a macro:
  Lambda-list: ((MNAME PACKAGE-LIST &REST SYMBOL-TYPES) &BODY BODY)
  Documentation:
    Within the lexical scope of the body forms, MNAME is defined via macrolet
    such that successive invocations of (MNAME) will return the symbols, one by
    one, from the packages in PACKAGE-LIST. SYMBOL-TYPES may be any
    of :INHERITED :EXTERNAL :INTERNAL.
  Source file: SYS:SRC;CODE;TARGET-PACKAGE.LISP

```

### `WITH-SIMPLE-RESTART`

```lisp
COMMON-LISP:WITH-SIMPLE-RESTART
  [symbol]

WITH-SIMPLE-RESTART names a macro:
  Lambda-list: ((RESTART-NAME FORMAT-STRING &REST FORMAT-ARGUMENTS)
                &BODY FORMS)
  Documentation:
    (WITH-SIMPLE-RESTART (restart-name format-string format-arguments)
       body)
       If restart-name is not invoked, then all values returned by forms are
       returned. If control is transferred to this restart, it immediately
       returns the values NIL and T.
  Source file: SYS:SRC;CODE;RESTART.LISP

```

### `WITH-SLOTS`

```lisp
COMMON-LISP:WITH-SLOTS
  [symbol]

WITH-SLOTS names a macro:
  Lambda-list: (SLOTS INSTANCE &BODY BODY)
  Source file: SYS:SRC;PCL;BOOT.LISP

```

### `WITH-STANDARD-IO-SYNTAX`

```lisp
COMMON-LISP:WITH-STANDARD-IO-SYNTAX
  [symbol]

WITH-STANDARD-IO-SYNTAX names a macro:
  Lambda-list: (&BODY BODY)
  Documentation:
    Bind the reader and printer control variables to values that enable READ
       to reliably read the results of PRINT. These values are:
    
             *PACKAGE*                        the COMMON-LISP-USER package
             *PRINT-ARRAY*                    T
             *PRINT-BASE*                     10
             *PRINT-CASE*                     :UPCASE
             *PRINT-CIRCLE*                   NIL
             *PRINT-ESCAPE*                   T
             *PRINT-GENSYM*                   T
             *PRINT-LENGTH*                   NIL
             *PRINT-LEVEL*                    NIL
             *PRINT-LINES*                    NIL
             *PRINT-MISER-WIDTH*              NIL
             *PRINT-PPRINT-DISPATCH*          the standard pprint dispatch table
             *PRINT-PRETTY*                   NIL
             *PRINT-RADIX*                    NIL
             *PRINT-READABLY*                 T
             *PRINT-RIGHT-MARGIN*             NIL
             *READ-BASE*                      10
             *READ-DEFAULT-FLOAT-FORMAT*      SINGLE-FLOAT
             *READ-EVAL*                      T
             *READ-SUPPRESS*                  NIL
             *READTABLE*                      the standard readtable
      SB-EXT:*SUPPRESS-PRINT-ERRORS*          NIL
      SB-EXT:*PRINT-VECTOR-LENGTH*            NIL

  Source file: SYS:SRC;CODE;ANSI-STREAM.LISP

```


## primitive type-specifier

### Symbol list

1. [ARRAY](#array)
2. [ATOM](#atom)
3. [BASE-CHAR](#base-char)
4. [BASE-STRING](#base-string)
5. [BIT](#bit)
6. [BIT-VECTOR](#bit-vector)
7. [COMPILED-FUNCTION](#compiled-function)
8. [COMPLEX](#complex)
9. [CONS](#cons)
10. [DOUBLE-FLOAT](#double-float)
11. [EXTENDED-CHAR](#extended-char)
12. [FLOAT](#float)
13. [FUNCTION](#function)
14. [INTEGER](#integer)
15. [KEYWORD](#keyword)
16. [LONG-FLOAT](#long-float)
17. [RATIONAL](#rational)
18. [REAL](#real)
19. [SHORT-FLOAT](#short-float)
20. [SIMPLE-ARRAY](#simple-array)
21. [SIMPLE-BASE-STRING](#simple-base-string)
22. [SIMPLE-BIT-VECTOR](#simple-bit-vector)
23. [SIMPLE-STRING](#simple-string)
24. [SIMPLE-VECTOR](#simple-vector)
25. [SINGLE-FLOAT](#single-float)
26. [STANDARD-CHAR](#standard-char)
27. [STRING](#string)
28. [VECTOR](#vector)


### `ARRAY`

```lisp
COMMON-LISP:ARRAY
  [symbol]

ARRAY names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:ARRAY>:
  Class precedence-list: ARRAY, T
  Direct superclasses: T
  Direct subclasses: SIMPLE-ARRAY, VECTOR
  Sealed.
  No direct slots.

ARRAY names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (ELEMENT-TYPE (QUOTE *))
                (DIMENSIONS (QUOTE *)))

```

### `ATOM`

```lisp
COMMON-LISP:ATOM
  [symbol]

ATOM names a compiled function:
  Lambda-list: (OBJECT)
  Declared type: (FUNCTION (T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return true if OBJECT is an ATOM, and NIL otherwise.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;PRED.LISP

ATOM names a primitive type-specifier:
  Lambda-list: ()

```

### `BASE-CHAR`

```lisp
COMMON-LISP:BASE-CHAR
  [symbol]

BASE-CHAR names a primitive type-specifier:
  Lambda-list: ()

```

### `BASE-STRING`

```lisp
COMMON-LISP:BASE-STRING
  [symbol]

BASE-STRING names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:BASE-STRING>:
  Class precedence-list: BASE-STRING, STRING, VECTOR, ARRAY, SEQUENCE, T
  Direct superclasses: STRING
  Direct subclasses: SIMPLE-BASE-STRING
  Sealed.
  No direct slots.

BASE-STRING names a primitive type-specifier:
  Lambda-list: (&OPTIONAL SIZE)

```

### `BIT`

```lisp
COMMON-LISP:BIT
  [symbol]

BIT names a compiled function:
  Lambda-list: (BIT-ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION ((ARRAY BIT) &REST (UNSIGNED-BYTE 45))
                  (VALUES BIT &OPTIONAL))
  Derived type: (FUNCTION ((ARRAY BIT) &REST T) (VALUES BIT &OPTIONAL))
  Documentation:
    Return the bit from the BIT-ARRAY at the specified SUBSCRIPTS.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

(SETF BIT) names a compiled function:
  Lambda-list: (NEW-VALUE BIT-ARRAY &REST SUBSCRIPTS)
  Declared type: (FUNCTION (BIT (ARRAY BIT) &REST (UNSIGNED-BYTE 45))
                  (VALUES BIT &OPTIONAL))
  Derived type: (FUNCTION (BIT (ARRAY BIT) &REST T)
                 (VALUES BIT &OPTIONAL))
  Source file: SYS:SRC;CODE;ARRAY.LISP

BIT names a primitive type-specifier:
  Lambda-list: ()

```

### `BIT-VECTOR`

```lisp
COMMON-LISP:BIT-VECTOR
  [symbol]

BIT-VECTOR names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:BIT-VECTOR>:
  Class precedence-list: BIT-VECTOR, VECTOR, ARRAY, SEQUENCE, T
  Direct superclasses: VECTOR
  Direct subclasses: SIMPLE-BIT-VECTOR
  Sealed.
  No direct slots.

BIT-VECTOR names a primitive type-specifier:
  Lambda-list: (&OPTIONAL SIZE)

```

### `COMPILED-FUNCTION`

```lisp
COMMON-LISP:COMPILED-FUNCTION
  [symbol]

COMPILED-FUNCTION names a primitive type-specifier:
  Lambda-list: ()

```

### `COMPLEX`

```lisp
COMMON-LISP:COMPLEX
  [symbol]

COMPLEX names a compiled function:
  Lambda-list: (REALPART &OPTIONAL (IMAGPART 0))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES NUMBER &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR COMPLEX RATIONAL) &OPTIONAL))
  Documentation:
    Return a complex number with the specified real and imaginary components.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

COMPLEX names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:COMPLEX>:
  Class precedence-list: COMPLEX, NUMBER, T
  Direct superclasses: NUMBER
  Direct subclasses: SB-KERNEL:COMPLEX-DOUBLE-FLOAT,
                     SB-KERNEL:COMPLEX-SINGLE-FLOAT
  Sealed.
  No direct slots.

COMPLEX names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (TYPESPEC (QUOTE *)))

```

### `CONS`

```lisp
COMMON-LISP:CONS
  [symbol]

CONS names a compiled function:
  Lambda-list: (SE1 SE2)
  Declared type: (FUNCTION (T T) (VALUES CONS &OPTIONAL))
  Documentation:
    Return a list with SE1 as the CAR and SE2 as the CDR.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;LIST.LISP

CONS names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:CONS>:
  Class precedence-list: CONS, LIST, SEQUENCE, T
  Direct superclasses: LIST
  No subclasses.
  Sealed.
  No direct slots.

CONS names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (CAR-TYPE-SPEC (QUOTE *))
                (CDR-TYPE-SPEC (QUOTE *)))

```

### `DOUBLE-FLOAT`

```lisp
COMMON-LISP:DOUBLE-FLOAT
  [symbol]

DOUBLE-FLOAT names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:DOUBLE-FLOAT>:
  Class precedence-list: DOUBLE-FLOAT, FLOAT, REAL, NUMBER, T
  Direct superclasses: FLOAT
  No subclasses.
  Sealed.
  No direct slots.

DOUBLE-FLOAT names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (LOW (QUOTE *)) (HIGH (QUOTE *)))

```

### `EXTENDED-CHAR`

```lisp
COMMON-LISP:EXTENDED-CHAR
  [symbol]

EXTENDED-CHAR names a primitive type-specifier:
  Documentation:
    Type of CHARACTERs that aren't BASE-CHARs.
  Lambda-list: ()

```

### `FLOAT`

```lisp
COMMON-LISP:FLOAT
  [symbol]

FLOAT names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (OTHER NIL OTHERP))
  Declared type: (FUNCTION (REAL &OPTIONAL FLOAT)
                  (VALUES FLOAT &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T) (VALUES FLOAT &OPTIONAL))
  Documentation:
    Converts any REAL to a float. If OTHER is not provided, it returns a
      SINGLE-FLOAT if NUMBER is not already a FLOAT. If OTHER is provided, the
      result is the same float format as OTHER.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

FLOAT names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:FLOAT>:
  Class precedence-list: FLOAT, REAL, NUMBER, T
  Direct superclasses: REAL
  Direct subclasses: DOUBLE-FLOAT, SINGLE-FLOAT
  Sealed.
  No direct slots.

FLOAT names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (LOW (QUOTE *)) (HIGH (QUOTE *)))

```

### `FUNCTION`

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

### `INTEGER`

```lisp
COMMON-LISP:INTEGER
  [symbol]

INTEGER names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:INTEGER>:
  Class precedence-list: INTEGER, RATIONAL, REAL, NUMBER, T
  Direct superclasses: RATIONAL
  Direct subclasses: BIGNUM, FIXNUM
  Sealed.
  No direct slots.

INTEGER names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (LOW (QUOTE *)) (HIGH (QUOTE *)))

```

### `KEYWORD`

```lisp
COMMON-LISP:KEYWORD
  [symbol]

KEYWORD names a primitive type-specifier:
  Lambda-list: ()

```

### `LONG-FLOAT`

```lisp
COMMON-LISP:LONG-FLOAT
  [symbol]

LONG-FLOAT names a primitive type-specifier:
  Lambda-list: (&OPTIONAL LOW HIGH)

```

### `RATIONAL`

```lisp
COMMON-LISP:RATIONAL
  [symbol]

RATIONAL names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION (REAL) (VALUES RATIONAL &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES RATIONAL &OPTIONAL))
  Documentation:
    RATIONAL produces a rational number for any real numeric argument. This is
      more efficient than RATIONALIZE, but it assumes that floating-point is
      completely accurate, giving a result that isn't as pretty.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;FLOAT.LISP

RATIONAL names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:RATIONAL>:
  Class precedence-list: RATIONAL, REAL, NUMBER, T
  Direct superclasses: REAL
  Direct subclasses: INTEGER, RATIO
  Sealed.
  No direct slots.

RATIONAL names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (LOW (QUOTE *)) (HIGH (QUOTE *)))

```

### `REAL`

```lisp
COMMON-LISP:REAL
  [symbol]

REAL names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:REAL>:
  Class precedence-list: REAL, NUMBER, T
  Direct superclasses: NUMBER
  Direct subclasses: FLOAT, RATIONAL
  Sealed.
  No direct slots.

REAL names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (LOW (QUOTE *)) (HIGH (QUOTE *)))

```

### `SHORT-FLOAT`

```lisp
COMMON-LISP:SHORT-FLOAT
  [symbol]

SHORT-FLOAT names a primitive type-specifier:
  Lambda-list: (&OPTIONAL LOW HIGH)

```

### `SIMPLE-ARRAY`

```lisp
COMMON-LISP:SIMPLE-ARRAY
  [symbol]

SIMPLE-ARRAY names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:SIMPLE-ARRAY>:
  Class precedence-list: SIMPLE-ARRAY, ARRAY, T
  Direct superclasses: ARRAY
  Direct subclasses: SB-KERNEL::SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-DOUBLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-FIXNUM,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-16,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-32,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-64,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-8,
                     SB-KERNEL::SIMPLE-ARRAY-SINGLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-15,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-16,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-2,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-31,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-32,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-4,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-63,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-64,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-7,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-8,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-FIXNUM,
                     SIMPLE-BIT-VECTOR, SIMPLE-STRING, SIMPLE-VECTOR
  Sealed.
  No direct slots.

SIMPLE-ARRAY names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (ELEMENT-TYPE (QUOTE *))
                (DIMENSIONS (QUOTE *)))

```

### `SIMPLE-BASE-STRING`

```lisp
COMMON-LISP:SIMPLE-BASE-STRING
  [symbol]

SIMPLE-BASE-STRING names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:SIMPLE-BASE-STRING>:
  Class precedence-list: SIMPLE-BASE-STRING, BASE-STRING, SIMPLE-STRING,
                         STRING, VECTOR, SIMPLE-ARRAY, ARRAY, SEQUENCE,
                         T
  Direct superclasses: BASE-STRING, SIMPLE-STRING
  No subclasses.
  Sealed.
  No direct slots.

SIMPLE-BASE-STRING names a primitive type-specifier:
  Lambda-list: (&OPTIONAL SIZE)

```

### `SIMPLE-BIT-VECTOR`

```lisp
COMMON-LISP:SIMPLE-BIT-VECTOR
  [symbol]

SIMPLE-BIT-VECTOR names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:SIMPLE-BIT-VECTOR>:
  Class precedence-list: SIMPLE-BIT-VECTOR, BIT-VECTOR, VECTOR,
                         SIMPLE-ARRAY, ARRAY, SEQUENCE, T
  Direct superclasses: BIT-VECTOR, SIMPLE-ARRAY
  No subclasses.
  Sealed.
  No direct slots.

SIMPLE-BIT-VECTOR names a primitive type-specifier:
  Lambda-list: (&OPTIONAL SIZE)

```

### `SIMPLE-STRING`

```lisp
COMMON-LISP:SIMPLE-STRING
  [symbol]

SIMPLE-STRING names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:SIMPLE-STRING>:
  Class precedence-list: SIMPLE-STRING, STRING, VECTOR, SIMPLE-ARRAY,
                         ARRAY, SEQUENCE, T
  Direct superclasses: STRING, SIMPLE-ARRAY
  Direct subclasses: SIMPLE-BASE-STRING,
                     SB-KERNEL:SIMPLE-CHARACTER-STRING
  Sealed.
  No direct slots.

SIMPLE-STRING names a primitive type-specifier:
  Lambda-list: (&OPTIONAL SIZE)

```

### `SIMPLE-VECTOR`

```lisp
COMMON-LISP:SIMPLE-VECTOR
  [symbol]

SIMPLE-VECTOR names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:SIMPLE-VECTOR>:
  Class precedence-list: SIMPLE-VECTOR, VECTOR, SIMPLE-ARRAY, ARRAY,
                         SEQUENCE, T
  Direct superclasses: VECTOR, SIMPLE-ARRAY
  No subclasses.
  Sealed.
  No direct slots.

SIMPLE-VECTOR names a primitive type-specifier:
  Lambda-list: (&OPTIONAL SIZE)

```

### `SINGLE-FLOAT`

```lisp
COMMON-LISP:SINGLE-FLOAT
  [symbol]

SINGLE-FLOAT names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:SINGLE-FLOAT>:
  Class precedence-list: SINGLE-FLOAT, FLOAT, REAL, NUMBER, T
  Direct superclasses: FLOAT
  No subclasses.
  Sealed.
  No direct slots.

SINGLE-FLOAT names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (LOW (QUOTE *)) (HIGH (QUOTE *)))

```

### `STANDARD-CHAR`

```lisp
COMMON-LISP:STANDARD-CHAR
  [symbol]

STANDARD-CHAR names a primitive type-specifier:
  Documentation:
    Type corresponding to the characters required by the standard.
  Lambda-list: ()

```

### `STRING`

```lisp
COMMON-LISP:STRING
  [symbol]

STRING names a compiled function:
  Lambda-list: (X)
  Declared type: (FUNCTION ((OR STRING SYMBOL CHARACTER))
                  (VALUES STRING &OPTIONAL))
  Derived type: (FUNCTION (T) (VALUES STRING &OPTIONAL))
  Documentation:
    Coerces X into a string. If X is a string, X is returned. If X is a
       symbol, its name is returned. If X is a character then a one element
       string containing that character is returned. If X cannot be coerced
       into a string, an error occurs.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

STRING names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:STRING>:
  Class precedence-list: STRING, VECTOR, ARRAY, SEQUENCE, T
  Direct superclasses: VECTOR
  Direct subclasses: BASE-STRING, SB-KERNEL::CHARACTER-STRING,
                     SIMPLE-STRING
  Sealed.
  No direct slots.

STRING names a primitive type-specifier:
  Lambda-list: (&OPTIONAL SIZE)

```

### `VECTOR`

```lisp
COMMON-LISP:VECTOR
  [symbol]

VECTOR names a compiled function:
  Lambda-list: (&REST OBJECTS)
  Declared type: (FUNCTION * (VALUES SIMPLE-VECTOR &OPTIONAL))
  Derived type: (FUNCTION (&REST T) (VALUES SIMPLE-VECTOR &OPTIONAL))
  Documentation:
    Construct a SIMPLE-VECTOR from the given objects.
  Known attributes: flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;ARRAY.LISP

VECTOR names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:VECTOR>:
  Class precedence-list: VECTOR, ARRAY, SEQUENCE, T
  Direct superclasses: ARRAY, SEQUENCE
  Direct subclasses: BIT-VECTOR,
                     SB-KERNEL::SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-DOUBLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-FIXNUM,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-16,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-32,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-64,
                     SB-KERNEL::SIMPLE-ARRAY-SIGNED-BYTE-8,
                     SB-KERNEL::SIMPLE-ARRAY-SINGLE-FLOAT,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-15,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-16,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-2,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-31,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-32,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-4,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-63,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-64,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-7,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-BYTE-8,
                     SB-KERNEL::SIMPLE-ARRAY-UNSIGNED-FIXNUM,
                     SIMPLE-VECTOR, STRING, SB-KERNEL::VECTOR-NIL
  Sealed.
  No direct slots.

VECTOR names a primitive type-specifier:
  Lambda-list: (&OPTIONAL ELEMENT-TYPE SIZE)

```


## constant variable

### Symbol list

1. [ARRAY-DIMENSION-LIMIT](#array-dimension-limit)
2. [ARRAY-RANK-LIMIT](#array-rank-limit)
3. [ARRAY-TOTAL-SIZE-LIMIT](#array-total-size-limit)
4. [BOOLE-1](#boole-1)
5. [BOOLE-2](#boole-2)
6. [BOOLE-AND](#boole-and)
7. [BOOLE-ANDC1](#boole-andc1)
8. [BOOLE-ANDC2](#boole-andc2)
9. [BOOLE-C1](#boole-c1)
10. [BOOLE-C2](#boole-c2)
11. [BOOLE-CLR](#boole-clr)
12. [BOOLE-EQV](#boole-eqv)
13. [BOOLE-IOR](#boole-ior)
14. [BOOLE-NAND](#boole-nand)
15. [BOOLE-NOR](#boole-nor)
16. [BOOLE-ORC1](#boole-orc1)
17. [BOOLE-ORC2](#boole-orc2)
18. [BOOLE-SET](#boole-set)
19. [BOOLE-XOR](#boole-xor)
20. [CALL-ARGUMENTS-LIMIT](#call-arguments-limit)
21. [CHAR-CODE-LIMIT](#char-code-limit)
22. [DOUBLE-FLOAT-EPSILON](#double-float-epsilon)
23. [DOUBLE-FLOAT-NEGATIVE-EPSILON](#double-float-negative-epsilon)
24. [INTERNAL-TIME-UNITS-PER-SECOND](#internal-time-units-per-second)
25. [LAMBDA-LIST-KEYWORDS](#lambda-list-keywords)
26. [LAMBDA-PARAMETERS-LIMIT](#lambda-parameters-limit)
27. [LEAST-NEGATIVE-DOUBLE-FLOAT](#least-negative-double-float)
28. [LEAST-NEGATIVE-LONG-FLOAT](#least-negative-long-float)
29. [LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT](#least-negative-normalized-double-float)
30. [LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT](#least-negative-normalized-long-float)
31. [LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT](#least-negative-normalized-short-float)
32. [LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT](#least-negative-normalized-single-float)
33. [LEAST-NEGATIVE-SHORT-FLOAT](#least-negative-short-float)
34. [LEAST-NEGATIVE-SINGLE-FLOAT](#least-negative-single-float)
35. [LEAST-POSITIVE-DOUBLE-FLOAT](#least-positive-double-float)
36. [LEAST-POSITIVE-LONG-FLOAT](#least-positive-long-float)
37. [LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT](#least-positive-normalized-double-float)
38. [LEAST-POSITIVE-NORMALIZED-LONG-FLOAT](#least-positive-normalized-long-float)
39. [LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT](#least-positive-normalized-short-float)
40. [LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT](#least-positive-normalized-single-float)
41. [LEAST-POSITIVE-SHORT-FLOAT](#least-positive-short-float)
42. [LEAST-POSITIVE-SINGLE-FLOAT](#least-positive-single-float)
43. [LONG-FLOAT-EPSILON](#long-float-epsilon)
44. [LONG-FLOAT-NEGATIVE-EPSILON](#long-float-negative-epsilon)
45. [MOST-NEGATIVE-DOUBLE-FLOAT](#most-negative-double-float)
46. [MOST-NEGATIVE-FIXNUM](#most-negative-fixnum)
47. [MOST-NEGATIVE-LONG-FLOAT](#most-negative-long-float)
48. [MOST-NEGATIVE-SHORT-FLOAT](#most-negative-short-float)
49. [MOST-NEGATIVE-SINGLE-FLOAT](#most-negative-single-float)
50. [MOST-POSITIVE-DOUBLE-FLOAT](#most-positive-double-float)
51. [MOST-POSITIVE-FIXNUM](#most-positive-fixnum)
52. [MOST-POSITIVE-LONG-FLOAT](#most-positive-long-float)
53. [MOST-POSITIVE-SHORT-FLOAT](#most-positive-short-float)
54. [MOST-POSITIVE-SINGLE-FLOAT](#most-positive-single-float)
55. [MULTIPLE-VALUES-LIMIT](#multiple-values-limit)
56. [NIL](#nil)
57. [PI](#pi)
58. [SHORT-FLOAT-EPSILON](#short-float-epsilon)
59. [SHORT-FLOAT-NEGATIVE-EPSILON](#short-float-negative-epsilon)
60. [SINGLE-FLOAT-EPSILON](#single-float-epsilon)
61. [SINGLE-FLOAT-NEGATIVE-EPSILON](#single-float-negative-epsilon)
62. [T](#t)


### `ARRAY-DIMENSION-LIMIT`

```lisp
COMMON-LISP:ARRAY-DIMENSION-LIMIT
  [symbol]

ARRAY-DIMENSION-LIMIT names a constant variable:
  Value: 35184372088832
  Documentation:
    the exclusive upper bound on any given dimension of an array

```

### `ARRAY-RANK-LIMIT`

```lisp
COMMON-LISP:ARRAY-RANK-LIMIT
  [symbol]

ARRAY-RANK-LIMIT names a constant variable:
  Value: 129
  Documentation:
    the exclusive upper bound on the rank of an array

```

### `ARRAY-TOTAL-SIZE-LIMIT`

```lisp
COMMON-LISP:ARRAY-TOTAL-SIZE-LIMIT
  [symbol]

ARRAY-TOTAL-SIZE-LIMIT names a constant variable:
  Value: 35184372088832
  Documentation:
    the exclusive upper bound on the total number of elements in an array

```

### `BOOLE-1`

```lisp
COMMON-LISP:BOOLE-1
  [symbol]

BOOLE-1 names a constant variable:
  Value: 2
  Documentation:
    Boole function op, makes BOOLE return integer1.

```

### `BOOLE-2`

```lisp
COMMON-LISP:BOOLE-2
  [symbol]

BOOLE-2 names a constant variable:
  Value: 3
  Documentation:
    Boole function op, makes BOOLE return integer2.

```

### `BOOLE-AND`

```lisp
COMMON-LISP:BOOLE-AND
  [symbol]

BOOLE-AND names a constant variable:
  Value: 6
  Documentation:
    Boole function op, makes BOOLE return logand of integer1 and integer2.

```

### `BOOLE-ANDC1`

```lisp
COMMON-LISP:BOOLE-ANDC1
  [symbol]

BOOLE-ANDC1 names a constant variable:
  Value: 12
  Documentation:
    Boole function op, makes BOOLE return logandc1 of integer1 and integer2.

```

### `BOOLE-ANDC2`

```lisp
COMMON-LISP:BOOLE-ANDC2
  [symbol]

BOOLE-ANDC2 names a constant variable:
  Value: 13
  Documentation:
    Boole function op, makes BOOLE return logandc2 of integer1 and integer2.

```

### `BOOLE-C1`

```lisp
COMMON-LISP:BOOLE-C1
  [symbol]

BOOLE-C1 names a constant variable:
  Value: 4
  Documentation:
    Boole function op, makes BOOLE return complement of integer1.

```

### `BOOLE-C2`

```lisp
COMMON-LISP:BOOLE-C2
  [symbol]

BOOLE-C2 names a constant variable:
  Value: 5
  Documentation:
    Boole function op, makes BOOLE return complement of integer2.

```

### `BOOLE-CLR`

```lisp
COMMON-LISP:BOOLE-CLR
  [symbol]

BOOLE-CLR names a constant variable:
  Value: 0
  Documentation:
    Boole function op, makes BOOLE return 0.

```

### `BOOLE-EQV`

```lisp
COMMON-LISP:BOOLE-EQV
  [symbol]

BOOLE-EQV names a constant variable:
  Value: 9
  Documentation:
    Boole function op, makes BOOLE return logeqv of integer1 and integer2.

```

### `BOOLE-IOR`

```lisp
COMMON-LISP:BOOLE-IOR
  [symbol]

BOOLE-IOR names a constant variable:
  Value: 7
  Documentation:
    Boole function op, makes BOOLE return logior of integer1 and integer2.

```

### `BOOLE-NAND`

```lisp
COMMON-LISP:BOOLE-NAND
  [symbol]

BOOLE-NAND names a constant variable:
  Value: 10
  Documentation:
    Boole function op, makes BOOLE return log nand of integer1 and integer2.

```

### `BOOLE-NOR`

```lisp
COMMON-LISP:BOOLE-NOR
  [symbol]

BOOLE-NOR names a constant variable:
  Value: 11
  Documentation:
    Boole function op, makes BOOLE return lognor of integer1 and integer2.

```

### `BOOLE-ORC1`

```lisp
COMMON-LISP:BOOLE-ORC1
  [symbol]

BOOLE-ORC1 names a constant variable:
  Value: 14
  Documentation:
    Boole function op, makes BOOLE return logorc1 of integer1 and integer2.

```

### `BOOLE-ORC2`

```lisp
COMMON-LISP:BOOLE-ORC2
  [symbol]

BOOLE-ORC2 names a constant variable:
  Value: 15
  Documentation:
    Boole function op, makes BOOLE return logorc2 of integer1 and integer2.

```

### `BOOLE-SET`

```lisp
COMMON-LISP:BOOLE-SET
  [symbol]

BOOLE-SET names a constant variable:
  Value: 1
  Documentation:
    Boole function op, makes BOOLE return -1.

```

### `BOOLE-XOR`

```lisp
COMMON-LISP:BOOLE-XOR
  [symbol]

BOOLE-XOR names a constant variable:
  Value: 8
  Documentation:
    Boole function op, makes BOOLE return logxor of integer1 and integer2.

```

### `CALL-ARGUMENTS-LIMIT`

```lisp
COMMON-LISP:CALL-ARGUMENTS-LIMIT
  [symbol]

CALL-ARGUMENTS-LIMIT names a constant variable:
  Value: 1073741824
  Documentation:
    The exclusive upper bound on the number of arguments which may be passed
      to a function, including &REST args.

```

### `CHAR-CODE-LIMIT`

```lisp
COMMON-LISP:CHAR-CODE-LIMIT
  [symbol]

CHAR-CODE-LIMIT names a constant variable:
  Value: 1114112
  Documentation:
    the upper exclusive bound on values produced by CHAR-CODE

```

### `DOUBLE-FLOAT-EPSILON`

```lisp
COMMON-LISP:DOUBLE-FLOAT-EPSILON
  [symbol]

DOUBLE-FLOAT-EPSILON names a constant variable:
  Value: 1.1102230246251568d-16

```

### `DOUBLE-FLOAT-NEGATIVE-EPSILON`

```lisp
COMMON-LISP:DOUBLE-FLOAT-NEGATIVE-EPSILON
  [symbol]

DOUBLE-FLOAT-NEGATIVE-EPSILON names a constant variable:
  Value: 5.551115123125784d-17

```

### `INTERNAL-TIME-UNITS-PER-SECOND`

```lisp
COMMON-LISP:INTERNAL-TIME-UNITS-PER-SECOND
  [symbol]

INTERNAL-TIME-UNITS-PER-SECOND names a constant variable:
  Value: 1000000
  Documentation:
    The number of internal time units that fit into a second. See
    GET-INTERNAL-REAL-TIME and GET-INTERNAL-RUN-TIME.

```

### `LAMBDA-LIST-KEYWORDS`

```lisp
COMMON-LISP:LAMBDA-LIST-KEYWORDS
  [symbol]

LAMBDA-LIST-KEYWORDS names a constant variable:
  Value: (&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY SB-INT:&MORE
          &OPTIONAL &REST &WHOLE)
  Documentation:
    A list of symbols used as lambda list keywords in SBCL.

```

### `LAMBDA-PARAMETERS-LIMIT`

```lisp
COMMON-LISP:LAMBDA-PARAMETERS-LIMIT
  [symbol]

LAMBDA-PARAMETERS-LIMIT names a constant variable:
  Value: 1073741824
  Documentation:
    The exclusive upper bound on the number of parameters which may be specified
      in a given lambda list. This is actually the limit on required and &OPTIONAL
      parameters. With &KEY and &AUX you can get more.

```

### `LEAST-NEGATIVE-DOUBLE-FLOAT`

```lisp
COMMON-LISP:LEAST-NEGATIVE-DOUBLE-FLOAT
  [symbol]

LEAST-NEGATIVE-DOUBLE-FLOAT names a constant variable:
  Value: -4.9406564584124654d-324

```

### `LEAST-NEGATIVE-LONG-FLOAT`

```lisp
COMMON-LISP:LEAST-NEGATIVE-LONG-FLOAT
  [symbol]

LEAST-NEGATIVE-LONG-FLOAT names a constant variable:
  Value: -4.9406564584124654d-324

```

### `LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT`

```lisp
COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
  [symbol]

LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT names a constant variable:
  Value: -2.2250738585072014d-308

```

### `LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT`

```lisp
COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
  [symbol]

LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT names a constant variable:
  Value: -2.2250738585072014d-308

```

### `LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT`

```lisp
COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT
  [symbol]

LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT names a constant variable:
  Value: -1.1754944e-38

```

### `LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT`

```lisp
COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
  [symbol]

LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT names a constant variable:
  Value: -1.1754944e-38

```

### `LEAST-NEGATIVE-SHORT-FLOAT`

```lisp
COMMON-LISP:LEAST-NEGATIVE-SHORT-FLOAT
  [symbol]

LEAST-NEGATIVE-SHORT-FLOAT names a constant variable:
  Value: -1.4012985e-45

```

### `LEAST-NEGATIVE-SINGLE-FLOAT`

```lisp
COMMON-LISP:LEAST-NEGATIVE-SINGLE-FLOAT
  [symbol]

LEAST-NEGATIVE-SINGLE-FLOAT names a constant variable:
  Value: -1.4012985e-45

```

### `LEAST-POSITIVE-DOUBLE-FLOAT`

```lisp
COMMON-LISP:LEAST-POSITIVE-DOUBLE-FLOAT
  [symbol]

LEAST-POSITIVE-DOUBLE-FLOAT names a constant variable:
  Value: 4.9406564584124654d-324

```

### `LEAST-POSITIVE-LONG-FLOAT`

```lisp
COMMON-LISP:LEAST-POSITIVE-LONG-FLOAT
  [symbol]

LEAST-POSITIVE-LONG-FLOAT names a constant variable:
  Value: 4.9406564584124654d-324

```

### `LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT`

```lisp
COMMON-LISP:LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
  [symbol]

LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT names a constant variable:
  Value: 2.2250738585072014d-308

```

### `LEAST-POSITIVE-NORMALIZED-LONG-FLOAT`

```lisp
COMMON-LISP:LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
  [symbol]

LEAST-POSITIVE-NORMALIZED-LONG-FLOAT names a constant variable:
  Value: 2.2250738585072014d-308

```

### `LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT`

```lisp
COMMON-LISP:LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT
  [symbol]

LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT names a constant variable:
  Value: 1.1754944e-38

```

### `LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT`

```lisp
COMMON-LISP:LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
  [symbol]

LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT names a constant variable:
  Value: 1.1754944e-38

```

### `LEAST-POSITIVE-SHORT-FLOAT`

```lisp
COMMON-LISP:LEAST-POSITIVE-SHORT-FLOAT
  [symbol]

LEAST-POSITIVE-SHORT-FLOAT names a constant variable:
  Value: 1.4012985e-45

```

### `LEAST-POSITIVE-SINGLE-FLOAT`

```lisp
COMMON-LISP:LEAST-POSITIVE-SINGLE-FLOAT
  [symbol]

LEAST-POSITIVE-SINGLE-FLOAT names a constant variable:
  Value: 1.4012985e-45

```

### `LONG-FLOAT-EPSILON`

```lisp
COMMON-LISP:LONG-FLOAT-EPSILON
  [symbol]

LONG-FLOAT-EPSILON names a constant variable:
  Value: 1.1102230246251568d-16

```

### `LONG-FLOAT-NEGATIVE-EPSILON`

```lisp
COMMON-LISP:LONG-FLOAT-NEGATIVE-EPSILON
  [symbol]

LONG-FLOAT-NEGATIVE-EPSILON names a constant variable:
  Value: 5.551115123125784d-17

```

### `MOST-NEGATIVE-DOUBLE-FLOAT`

```lisp
COMMON-LISP:MOST-NEGATIVE-DOUBLE-FLOAT
  [symbol]

MOST-NEGATIVE-DOUBLE-FLOAT names a constant variable:
  Value: -1.7976931348623157d308

```

### `MOST-NEGATIVE-FIXNUM`

```lisp
COMMON-LISP:MOST-NEGATIVE-FIXNUM
  [symbol]

MOST-NEGATIVE-FIXNUM names a constant variable:
  Value: -4611686018427387904
  Documentation:
    the fixnum closest in value to negative infinity

```

### `MOST-NEGATIVE-LONG-FLOAT`

```lisp
COMMON-LISP:MOST-NEGATIVE-LONG-FLOAT
  [symbol]

MOST-NEGATIVE-LONG-FLOAT names a constant variable:
  Value: -1.7976931348623157d308

```

### `MOST-NEGATIVE-SHORT-FLOAT`

```lisp
COMMON-LISP:MOST-NEGATIVE-SHORT-FLOAT
  [symbol]

MOST-NEGATIVE-SHORT-FLOAT names a constant variable:
  Value: -3.4028235e38

```

### `MOST-NEGATIVE-SINGLE-FLOAT`

```lisp
COMMON-LISP:MOST-NEGATIVE-SINGLE-FLOAT
  [symbol]

MOST-NEGATIVE-SINGLE-FLOAT names a constant variable:
  Value: -3.4028235e38

```

### `MOST-POSITIVE-DOUBLE-FLOAT`

```lisp
COMMON-LISP:MOST-POSITIVE-DOUBLE-FLOAT
  [symbol]

MOST-POSITIVE-DOUBLE-FLOAT names a constant variable:
  Value: 1.7976931348623157d308

```

### `MOST-POSITIVE-FIXNUM`

```lisp
COMMON-LISP:MOST-POSITIVE-FIXNUM
  [symbol]

MOST-POSITIVE-FIXNUM names a constant variable:
  Value: 4611686018427387903
  Documentation:
    the fixnum closest in value to positive infinity

```

### `MOST-POSITIVE-LONG-FLOAT`

```lisp
COMMON-LISP:MOST-POSITIVE-LONG-FLOAT
  [symbol]

MOST-POSITIVE-LONG-FLOAT names a constant variable:
  Value: 1.7976931348623157d308

```

### `MOST-POSITIVE-SHORT-FLOAT`

```lisp
COMMON-LISP:MOST-POSITIVE-SHORT-FLOAT
  [symbol]

MOST-POSITIVE-SHORT-FLOAT names a constant variable:
  Value: 3.4028235e38

```

### `MOST-POSITIVE-SINGLE-FLOAT`

```lisp
COMMON-LISP:MOST-POSITIVE-SINGLE-FLOAT
  [symbol]

MOST-POSITIVE-SINGLE-FLOAT names a constant variable:
  Value: 3.4028235e38

```

### `MULTIPLE-VALUES-LIMIT`

```lisp
COMMON-LISP:MULTIPLE-VALUES-LIMIT
  [symbol]

MULTIPLE-VALUES-LIMIT names a constant variable:
  Value: 1073741824
  Documentation:
    The exclusive upper bound on the number of multiple VALUES that you can
      return.

```

### `NIL`

```lisp
COMMON-LISP:NIL
  [null]

NIL names a constant variable:
  Value: NIL

```

### `PI`

```lisp
COMMON-LISP:PI
  [symbol]

PI names a constant variable:
  Value: 3.141592653589793d0

```

### `SHORT-FLOAT-EPSILON`

```lisp
COMMON-LISP:SHORT-FLOAT-EPSILON
  [symbol]

SHORT-FLOAT-EPSILON names a constant variable:
  Value: 5.960465e-8

```

### `SHORT-FLOAT-NEGATIVE-EPSILON`

```lisp
COMMON-LISP:SHORT-FLOAT-NEGATIVE-EPSILON
  [symbol]

SHORT-FLOAT-NEGATIVE-EPSILON names a constant variable:
  Value: 2.9802326e-8

```

### `SINGLE-FLOAT-EPSILON`

```lisp
COMMON-LISP:SINGLE-FLOAT-EPSILON
  [symbol]

SINGLE-FLOAT-EPSILON names a constant variable:
  Value: 5.960465e-8

```

### `SINGLE-FLOAT-NEGATIVE-EPSILON`

```lisp
COMMON-LISP:SINGLE-FLOAT-NEGATIVE-EPSILON
  [symbol]

SINGLE-FLOAT-NEGATIVE-EPSILON names a constant variable:
  Value: 2.9802326e-8

```

### `T`

```lisp
COMMON-LISP:T
  [symbol]

T names a constant variable:
  Value: T

T names the system-class #<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>:
  Class precedence-list: T
  Direct subclasses: PATHNAME, ARRAY, SIMD-PACK-256, SIMD-PACK, NUMBER,
                     SB-KERNEL::RANDOM-CLASS, SB-KERNEL:FDEFN,
                     SB-KERNEL:CODE-COMPONENT, WEAK-POINTER,
                     SYSTEM-AREA-POINTER, SYMBOL, CHARACTER,
                     SB-PCL::SLOT-OBJECT, SEQUENCE, STREAM, FUNCTION
  No direct slots.

```


## special operator

### Symbol list

1. [BLOCK](#block)
2. [CATCH](#catch)
3. [EVAL-WHEN](#eval-when)
4. [FLET](#flet)
5. [FUNCTION](#function)
6. [GO](#go)
7. [IF](#if)
8. [LABELS](#labels)
9. [LET](#let)
10. [LET*](#let*)
11. [LOAD-TIME-VALUE](#load-time-value)
12. [LOCALLY](#locally)
13. [MACROLET](#macrolet)
14. [MULTIPLE-VALUE-CALL](#multiple-value-call)
15. [MULTIPLE-VALUE-PROG1](#multiple-value-prog1)
16. [PROGN](#progn)
17. [PROGV](#progv)
18. [QUOTE](#quote)
19. [RETURN-FROM](#return-from)
20. [SETQ](#setq)
21. [SYMBOL-MACROLET](#symbol-macrolet)
22. [TAGBODY](#tagbody)
23. [THE](#the)
24. [THROW](#throw)
25. [UNWIND-PROTECT](#unwind-protect)


### `BLOCK`

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

### `CATCH`

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

### `EVAL-WHEN`

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

### `FLET`

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

### `FUNCTION`

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

### `GO`

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

### `IF`

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

### `LABELS`

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

### `LET`

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

### `LET*`

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

### `LOAD-TIME-VALUE`

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

### `LOCALLY`

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

### `MACROLET`

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

### `MULTIPLE-VALUE-CALL`

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

### `MULTIPLE-VALUE-PROG1`

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

### `PROGN`

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

### `PROGV`

```lisp
COMMON-LISP:PROGV
  [symbol]

PROGV names a special operator:
  Lambda-list: (VARS VALS &BODY BODY)
  Source file: SYS:SRC;COMPILER;IR2TRAN.LISP

```

### `QUOTE`

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

### `RETURN-FROM`

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

### `SETQ`

```lisp
COMMON-LISP:SETQ
  [symbol]

SETQ names a special operator:
  Lambda-list: (&WHOLE SOURCE &REST THINGS)
  Source file: SYS:SRC;COMPILER;IR1-TRANSLATORS.LISP

```

### `SYMBOL-MACROLET`

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

### `TAGBODY`

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

### `THE`

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

### `THROW`

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

### `UNWIND-PROTECT`

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


## type-specifier

### Symbol list

1. [BOOLEAN](#boolean)
2. [EQL](#eql)
3. [MOD](#mod)
4. [SIGNED-BYTE](#signed-byte)
5. [UNSIGNED-BYTE](#unsigned-byte)


### `BOOLEAN`

```lisp
COMMON-LISP:BOOLEAN
  [symbol]

BOOLEAN names a type-specifier:
  Lambda-list: ()
  Expansion: (MEMBER T NIL)

```

### `EQL`

```lisp
COMMON-LISP:EQL
  [symbol]

EQL names a compiled function:
  Lambda-list: (X Y)
  Declared type: (FUNCTION (T T) (VALUES BOOLEAN &OPTIONAL))
  Documentation:
    Return T if OBJ1 and OBJ2 represent the same object, otherwise NIL.
  Known attributes: foldable, flushable, unsafely-flushable, movable, predicate, commutative
  Source file: SYS:SRC;CODE;PRED.LISP

EQL names a type-specifier:
  Lambda-list: (N)

```

### `MOD`

```lisp
COMMON-LISP:MOD
  [symbol]

MOD names a compiled function:
  Lambda-list: (NUMBER DIVISOR)
  Declared type: (FUNCTION (REAL REAL) (VALUES REAL &OPTIONAL))
  Derived type: (FUNCTION (T T) (VALUES REAL &OPTIONAL))
  Documentation:
    Return second result of FLOOR.
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP

MOD names a type-specifier:
  Lambda-list: (N)

```

### `SIGNED-BYTE`

```lisp
COMMON-LISP:SIGNED-BYTE
  [symbol]

SIGNED-BYTE names a type-specifier:
  Lambda-list: (&OPTIONAL S)
  Expansion: INTEGER

```

### `UNSIGNED-BYTE`

```lisp
COMMON-LISP:UNSIGNED-BYTE
  [symbol]

UNSIGNED-BYTE names a type-specifier:
  Lambda-list: (&OPTIONAL S)
  Expansion: (INTEGER 0)

```

