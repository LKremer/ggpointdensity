#' Count Neighbors within a Radius (R Implementation)
#'
#' This function counts the number of neighboring points within a specified radius for each point in a given set of coordinates using an R implementation.
#'
#' @param x A numeric vector of x-coordinates of the points.
#' @param y A numeric vector of y-coordinates of the points.
#' @param r2 A numeric value representing the squared radius within which to search for neighboring points.
#' @param xy A numeric value representing the aspect ratio (usually the ratio of the y-scale to the x-scale).
#' @return A numeric vector where each element represents the count of neighboring points within the specified radius for each point.
count_neighbors <- function(x, y, r2, xy) {
  .Call("count_neighbors_", x, y, r2, xy, "ggpointdensity")
}

# Equivalent R code:
# count_neighbors_r <- function(x, y, r2, xy) {
#   yx <- 1 / xy
#   sapply(1:length(x), function(i) {
#     sum((yx * (x[i] - x)^2) + (xy * (y[i] - y)^2) < r2)
#   })
# }

#' @inherit geom_pointdensity
#'
#' @export
stat_pointdensity <- function(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  adjust = 1,
  na.rm = FALSE,
  method = "auto",
  method.args = list(),
  show.legend = NA,
  inherit.aes = TRUE
) {
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

StatPointdensity <- ggproto(
  "StatPointdensity",
  Stat,
  default_aes = aes(color = stat(density)),
  required_aes = c("x", "y"),

  compute_layer = function(self, data, params, layout) {
    # This function mostly copied from ggplot2's Stat
    ggplot2:::check_required_aesthetics(
      self$required_aes,
      c(names(data), names(params)),
      ggplot2:::snake_class(self)
    )

    # Make sure required_aes consists of the used set of aesthetics in case of
    # "|" notation in self$required_aes
    required_aes <- intersect(
      names(data),
      unlist(strsplit(self$required_aes, "|", fixed = TRUE))
    )

    data <- ggplot2:::remove_missing(
      data,
      params$na.rm,
      c(required_aes, self$non_missing_aes),
      ggplot2:::snake_class(self),
      finite = FALSE # Note that in ggplot2's Stat this is TRUE
    )

    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]

    args <- c(list(data = quote(data), scales = quote(scales)), params)
    ggplot2:::dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      tryCatch(do.call(self$compute_panel, args), error = function(e) {
        warning(glue::glue(
          "Computation failed in `{ggplot2:::snake_class(self)}()`:\n{e$message}"
        ))
        vctrs:::new_data_frame()
      })
    })
  },

  setup_params = function(data, params) {
    if (identical(params$method, "auto")) {
      # Use default nn correction for small datasets, kde2d for
      # larger. Based on size of the _largest_ group.
      max_group <- max(table(interaction(data$group, data$PANEL, drop = TRUE)))
      if (max_group > 20000) {
        message(paste0(
          "geom_pointdensity using method='kde2d' ",
          "due to large number of points (>20k)"
        ))
        params$method <- "kde2d"
      } else {
        params$method <- "default"
      }
    }

    params
  },

  compute_group = function(
    data,
    scales,
    adjust = 1,
    method = "auto",
    method.args = list()
  ) {
    finites <- is.finite(data$x) & is.finite(data$y)

    if (nrow(data[finites, ]) == 1) {
      data$density <- NA
    } else if (identical(method, "default")) {
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
        data$x,
        data$y,
        r2 = r2,
        xy = xy
      )
    } else if (identical(method, "kde2d")) {
      ddata <- data[finites, ]
      base.args <- list(
        x = ddata$x,
        y = ddata$y,
        lims = c(scales$x$dimension(), scales$y$dimension())
      )
      if (!is.element("n", names(method.args))) {
        method.args["n"] <- 100
      }
      if (!is.element("h", names(method.args))) {
        h <- c(MASS::bandwidth.nrd(ddata$x), MASS::bandwidth.nrd(ddata$y))
        h <- pmax(sqrt(.Machine$double.eps), h)
        method.args$h <- h * adjust
      }

      dens <- do.call(MASS::kde2d, c(base.args, method.args))
      # credits to Kamil Slowikowski:
      ix <- findInterval(data$x, dens$x)
      iy <- findInterval(data$y, dens$y)
      ii <- cbind(ix, iy)
      data$density[finites] <- dens$z[ii]
      data$density[!finites] <- min(dens$z)
    } else {
      if (is.character(method)) {
        method <- match.fun(method)
      }
      data$density <- do.call(method, c(method.args))
    }

    data$ndensity <- data$density / max(data$density)

    data
  }
)


#' A cross between a scatter plot and a 2D density plot
#'
#' @param method description
#' @param adjust description
#' @inheritParams ggplot2::geom_point
#'
#' @export
#'
#' @inheritSection ggplot2::geom_point Aesthetics
#'
#' @examples
#' library(ggpointdensity)
#' library(ggplot2)
#' library(dplyr)
#'
#' # generate some toy data
#' dat <- bind_rows(
#'   tibble(x = rnorm(7000, sd = 1),
#'          y = rnorm(7000, sd = 10),
#'          group = "foo"),
#'   tibble(x = rnorm(3000, mean = 1, sd = .5),
#'          y = rnorm(3000, mean = 7, sd = 5),
#'          group = "bar"))
#'
#' # plot it with geom_pointdensity()
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity()
#'
#' # adjust the smoothing bandwidth,
#' # i.e. the radius around the points
#' # in which neighbors are counted
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity(adjust = .1)
#'
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity(adjust = 4)
#'
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity(adjust = 4) +
#'   scale_colour_continuous(low = "red", high = "black")
#'
#' # I recommend the viridis package
#' # for a more useful color scale
#' library(viridis)
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity() +
#'   scale_color_viridis()
#'
#' # Of course you can combine the geom with standard
#' # ggplot2 features such as facets...
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity() +
#'   scale_color_viridis() +
#'   facet_wrap(~ group)
#'
#' # ... or point shape and size:
#' dat_subset <- sample_frac(dat, .1)  #' smaller data set
#' ggplot(data = dat_subset, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity(size = 3, shape = 17) +
#'   scale_color_viridis()
#'
#' # Zooming into the axis works as well, keep in mind
#' # that xlim() and ylim() affect the density since they
#' # remove data points.
#' # It may be better to use coord_cartesian() instead.
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity() +
#'   scale_color_viridis() +
#'   xlim(c(-1, 3)) + ylim(c(-5, 15))
#'
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity() +
#'   scale_color_viridis() +
#'   coord_cartesian(xlim = c(-1, 3), ylim = c(-5, 15))
geom_pointdensity <- function(
  mapping = NULL,
  data = NULL,
  stat = "pointdensity",
  position = "identity",
  ...,
  method = "auto",
  adjust = 1,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
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
