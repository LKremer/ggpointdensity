#' @import ggplot2


count_neighbors <- function(x, y, r2, xy) {
  .Call("count_neighbors_", x, y, r2, xy, "ggpointdensity")
}

#' Implementation of count_neighbors in R. Not actually used, just for clarity
count_neighbors_r <- function(x, y, r2, xy) {
  yx <- 1 / xy
  sapply(1:length(x), function(i) {
    sum((yx * (x[i] - x) ^ 2) + (xy * (y[i] - y) ^ 2) < r2)
  })
}

stat_pointdensity <- function(mapping = NULL,
                              data = NULL,
                              geom = "point",
                              position = "identity",
                              ...,
                              adjust = 1,
                              na.rm = FALSE,
                              method = "auto",
                              method.args = list(),
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
      adjust = adjust,
      na.rm = na.rm,
      method = method,
      method.args = method.args,
      ...
    )
  )
}

StatPointdensity <- ggproto("StatPointdensity", Stat,
                            default_aes = aes(color = stat(density)),
                            required_aes = c("x", "y"),

                            setup_params = function(data, params) {
                              if (identical(params$method, "auto")) {
                                # Use default nn correction for small datasets, kde2d for
                                # larger. Based on size of the _largest_ group.
                                max_group <- max(table(interaction(data$group, data$PANEL, drop = TRUE)))
                                if (max_group > 20000) {
                                  message(paste0("geom_pointdensity using method='kde2d' ",
                                                 "due to large number of points (>20k)"))
                                  params$method <- "kde2d"
                                } else {
                                  params$method <- "default"
                                }
                              }

                              params
                            },

                            compute_group = function(data, scales, adjust = 1, method = "auto",
                                                     method.args = list()) {

                              if (identical(method, "default")) {

                                # find an appropriate bandwidth (radius), pretty ad-hoc:
                                xrange <- diff(scales$x$get_limits()) * adjust
                                yrange <- diff(scales$y$get_limits()) * adjust
                                r2 <- (xrange + yrange) / 70

                                # since x and y may be on different scales, we need a
                                # factor to weight x and y distances accordingly:
                                xy <- xrange / yrange

                                # counting the number of neighbors around each point,
                                # this will be used to color the points
                                data$density <- count_neighbors(
                                  data$x, data$y, r2 = r2, xy = xy)
                                data$ndensity <- data$density/max(data$density)

                              } else if (identical(method, "kde2d")) {

                                base.args <- list(
                                  x = data$x,
                                  y = data$y,
                                  lims = c(scales$x$dimension(), scales$y$dimension()))
                                if (!is.element("n", names(method.args))) {
                                  method.args["n"] <- 100
                                }
                                if (!is.element("h", names(method.args))) {
                                  h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
                                  method.args$h <- h * adjust
                                }

                                dens <- do.call(MASS::kde2d, c(base.args, method.args))
                                # credits to Kamil Slowikowski:
                                ix <- findInterval(data$x, dens$x)
                                iy <- findInterval(data$y, dens$y)
                                ii <- cbind(ix, iy)
                                data$density <- dens$z[ii]
                                data$ndensity <- dens$z[ii]/max(dens$z[ii])


                              } else {

                                if (is.character(method)) {
                                  method <- match.fun(method)
                                }
                                data$density <- do.call(method, c(method.args))
                                data$ndensity <- data$density/max(data$density)

                              }

                              data
                            }
)

geom_pointdensity <- function(mapping = NULL,
                              data = NULL,
                              stat = "pointdensity",
                              position = "identity",
                              ...,
                              method = "auto",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      na.rm = na.rm,
      ...
    )
  )
}
