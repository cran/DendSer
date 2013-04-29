

#include <Rinternals.h>
#include <R.h>

SEXP clpl(SEXP d, SEXP o,SEXP tar)
{ 
   SEXP res;

   PROTECT(res = allocVector(REALSXP, 1));
  int n = length(o),a;
  double *xd=REAL(d), *xtar=REAL(tar), *xres=REAL(res),r;
  int *xo=INTEGER(o);
  r = 0.0;
  for (int i =1; i < n; i++){
      a = xo[i] + n*(xo[i-1]-1) -1;	
	  r += xd[a]*xtar[i-1];
	  };
  xres[0]=r;
  UNPROTECT(1);
  return(res);
}
