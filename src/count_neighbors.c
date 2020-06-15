#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

void R_init_ggpointdensity(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

SEXP count_neighbors_( SEXP x, SEXP y, SEXP r2, SEXP xy ) {
  double r2p = REAL(r2)[0];
  double xyp = REAL(xy)[0];
  double yxp = 1.0 / xyp;
  int l = Rf_length(x);
  if( Rf_length(y) != l )
    error( "Vectors x and y differ in length." );
  SEXP res = Rf_allocVector( INTSXP, l );
  int *resp = INTEGER(res);
  double *xp = REAL(x);
  double *yp = REAL(y);
    for( int i = 0; i < l; i++ ) {
      int s = 0;
      double xi = xp[i];
      double yi = yp[i];
      for( int j = 0; j < l; j++ ) {\
        double xj = xp[j];
        double yj = yp[j];
        double dx = xi - xj;
        double dy = yi - yj;
        if((xi == xj && yi == yj) ||
           (xi == xj && xyp * dy * dy <= r2p) ||
           (yi == yj && xyp * dx * dx <= r2p) ||
           (yxp*dx*dx + xyp*dy*dy <= r2p) ){
           s++;
        }

      }
      resp[i] = s;
    }
    return res;
}
