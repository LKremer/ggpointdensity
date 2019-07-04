#' @import ggplot2


count_neighbors_r <- function( x, y, r2, xy, yx) {
   sapply( 1:length(x), function(i)
   sum( (yx*(x[i]-x)^2) + (xy*(y[i]-y)^2) < r2 ) )
}


#' @export
stat_pointdensity <- function(mapping = NULL, data = NULL,
                              geom = "point", position = "identity",
                              ...,
                              adjust = 1,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPointdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      adjust = 1,
      na.rm = na.rm,
      ...
    )
  )
}

StatPointdensity <- ggproto("StatPointdensity", Stat,
                            default_aes = aes(color = stat(n_neighbors)),
                            required_aes = c("x", "y"),

                            compute_group = function(data, scales, adjust = 1) {
                              adjust <- ggplot2:::dual_param(adjust, list(x = 1, y = 1))

                              xrange <- diff(scales$x$get_limits()) * adjust$x
                              yrange <- diff(scales$y$get_limits()) * adjust$y
                              xy <- xrange / yrange
                              yx <- yrange / xrange

                              r2 <- (xrange + yrange) / 70

                              count_neighbors_c <- inline::cfunction(
                                signature( x="numeric", y="numeric", r2="numeric",
                                           xy="numeric", yx="numeric" ), '
        double r2p = REAL(r2)[0];
        double xyp = REAL(xy)[0];
        double yxp = REAL(yx)[0];
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
              if( yxp*dx*dx + xyp*dy*dy <= r2p )
                 s++;
           }
           resp[i] = s;
        }
        return res;
    ')

                              neigh <- count_neighbors_c(data$x, data$y, r2 = r2,
                                                         xy = xy, yx = yx)
                              out <- data.frame(x = data$x,
                                                y = data$y,
                                                n_neighbors = neigh)

                              out
                            }
)

#' @export
geom_pointdensity <- function(mapping = NULL, data = NULL,
                              stat = "pointdensity", position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
