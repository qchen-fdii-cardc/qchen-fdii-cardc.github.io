/*
 * MATLAB Compiler: 23.2 (R2023b)
 * Date: Sat May 24 09:12:22 2025
 * Arguments:
 * "-B""macro_default""-W""lib:easy,version=1.0""-T""link:lib""-d""D:\writing\qc
 * hen-fdii-cardc.github.io\static\matlab\easy_dll\easy\for_testing""-v""D:\writ
 * ing\qchen-fdii-cardc.github.io\static\matlab\easy_dll\easy.m"
 */

#ifndef easy_h
#define easy_h 1

#if defined(__cplusplus) && !defined(mclmcrrt_h) && defined(__linux__)
#  pragma implementation "mclmcrrt.h"
#endif
#include "mclmcrrt.h"
#ifdef __cplusplus
extern "C" { // sbcheck:ok:extern_c
#endif

/* This symbol is defined in shared libraries. Define it here
 * (to nothing) in case this isn't a shared library. 
 */
#ifndef LIB_easy_C_API 
#define LIB_easy_C_API /* No special import/export declaration */
#endif

/* GENERAL LIBRARY FUNCTIONS -- START */

extern LIB_easy_C_API 
bool MW_CALL_CONV easyInitializeWithHandlers(
       mclOutputHandlerFcn error_handler, 
       mclOutputHandlerFcn print_handler);

extern LIB_easy_C_API 
bool MW_CALL_CONV easyInitialize(void);
extern LIB_easy_C_API 
void MW_CALL_CONV easyTerminate(void);

extern LIB_easy_C_API 
void MW_CALL_CONV easyPrintStackTrace(void);

/* GENERAL LIBRARY FUNCTIONS -- END */

/* C INTERFACE -- MLX WRAPPERS FOR USER-DEFINED MATLAB FUNCTIONS -- START */

extern LIB_easy_C_API 
bool MW_CALL_CONV mlxEasy(int nlhs, mxArray *plhs[], int nrhs, mxArray *prhs[]);

/* C INTERFACE -- MLX WRAPPERS FOR USER-DEFINED MATLAB FUNCTIONS -- END */

/* C INTERFACE -- MLF WRAPPERS FOR USER-DEFINED MATLAB FUNCTIONS -- START */

extern LIB_easy_C_API bool MW_CALL_CONV mlfEasy(int nargout, mxArray** r, mxArray* a, mxArray* b);

#ifdef __cplusplus
}
#endif
/* C INTERFACE -- MLF WRAPPERS FOR USER-DEFINED MATLAB FUNCTIONS -- END */

#endif
