
#include <Rinternals.h>
#include <R.h>


SEXP carct(SEXP d, SEXP o,SEXP tar)
{ SEXP res;
  PROTECT(res = allocVector(REALSXP, 1));
  int n = length(o),a,b;
  double *xd=REAL(d), *xtar=REAL(tar), *xres=REAL(res), r;
  int *xo=INTEGER(o);

  r = 0.0;
  
  for (int i =0; i < n-1 ; i++){
  	for(int j =i+1; j < n; j++){
      a = xo[i] + n*(xo[j]-1) -1;
      b = i + n*j;	
	  r += xd[a]*xtar[b];
	  };
  };
  xres[0]=r;
  UNPROTECT(1);
  return(res);
}

