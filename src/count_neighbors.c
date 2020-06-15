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
      for( int j = 0; j < l; j++ ) {
        double dx = xi - xp[j];
        double dy = yi - yp[j];
        if(R_FINITE(dx) && R_FINITE(dy)){
          if( yxp*dx*dx + xyp*dy*dy <= r2p )
            s++;
        }else{
          if((! R_FINITE(dx) && xyp * dy * dy < r2p) ||
             (! R_FINITE(dy) && xyp * dx * dx < r2p) ){
            s++;
          }
        }
      }
      resp[i] = s;
    }
    return res;
}
