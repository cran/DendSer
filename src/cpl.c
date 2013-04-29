

#include <Rinternals.h>
#include <R.h>

SEXP cpl(SEXP d, SEXP o)
{ 
   SEXP res;

   PROTECT(res = allocVector(REALSXP, 1));
  int n = length(o),a,m;
  double *xd=REAL(d), *xres=REAL(res),r;
  int *xo=INTEGER(o);
  m = sqrt(length(d));
  r = 0.0;
  for (int i =1; i < n; i++){
      a = xo[i] + m*(xo[i-1]-1) -1;	
	  r += xd[a];
	  };
  xres[0]=r;
  UNPROTECT(1);
  return(res);
}
