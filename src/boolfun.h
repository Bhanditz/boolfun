#include <iostream>
using namespace std;
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>


/* Entry points called from the R functions */
extern "C" {
SEXP    mobiusTransform     ( SEXP truth_table,  SEXP nb_var );
SEXP    walshTransform      ( SEXP truth_table,  SEXP nb_var );
SEXP    algebraicImmunity   ( SEXP truth_table,  SEXP nb_var );
SEXP    degree              ( SEXP coeff_of_anf, SEXP nb_var ); 
}

