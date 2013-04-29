

#include <Rinternals.h>
#include <R.h>


SEXP cbar(SEXP d, SEXP o, SEXP k)
{ SEXP res;
   PROTECT(res = allocVector(REALSXP, 1));
  int n = length(o),a,b,kval;
  double *xd=REAL(d), *xres=REAL(res),r;
  int *xo=INTEGER(o), *xk=INTEGER(k);
  
  r = 0.0;
  kval = xk[0];
  for (int i =0; i < n -1; i++){
  	b = kval+1;
  	for(int j =i+1; b > 1 && j < n; j++){
      a = xo[i] + n*(xo[j]-1) -1;	
	  r += xd[a]*--b;
	  };
  };
  xres[0]=r;
  UNPROTECT(1);
  return(res);
}

