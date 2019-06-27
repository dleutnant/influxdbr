#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

// destructive NA filler of nested list of lists
extern SEXP C_fill_nulls(SEXP lst) {
  if (TYPEOF(lst) == VECSXP) {
	R_xlen_t len = Rf_xlength(lst);
	for (R_xlen_t i = 0; i < len; i++) {
	  SEXP el = VECTOR_ELT(lst, i);
	  if (el == R_NilValue)
		SET_VECTOR_ELT(lst, i, Rf_ScalarLogical(NA_LOGICAL));
	  else if (TYPEOF(el) == VECSXP)
		SET_VECTOR_ELT(lst, i, C_fill_nulls(el));
	}
  } else {
	Rf_error("A (nested) list required");
  }
  return lst;
}

static const R_CallMethodDef CallEntries[] = {
  {"C_fill_nulls", (DL_FUNC) &C_fill_nulls, 1},
  {NULL, NULL, 0}
};

void R_init_influxdbr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
