#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP corCovBlock(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP extremeRank(SEXP, SEXP, SEXP);
extern SEXP projectionDepth(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP totalVariationDepth(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"corCovBlock",         (DL_FUNC) &corCovBlock,         8},
  {"extremeRank",         (DL_FUNC) &extremeRank,         3},
  {"projectionDepth",     (DL_FUNC) &projectionDepth,     7},
  {"totalVariationDepth", (DL_FUNC) &totalVariationDepth, 4},
  {NULL, NULL, 0}
};

void R_init_fdaoutlier(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
