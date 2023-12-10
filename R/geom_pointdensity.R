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


#' Wraps the user supplied Geom (typically GeomPoint) to add class "check_aspect_grob" and information about the aspect ratio assumed under which the densities were calculated to the grobs it draws
#' The check is injected by providing an S3 instance to the makeContext generic that is called by grid for each grob at render time (where the actual plot aspect ratio is finally known)
#' @import rlang
addCheckToGeom <- function(orig_geom, expected_aspect_ratio = 1) {
  if (is.null(orig_geom))
    cli::cli_abort("Can't create layer without a geom.", call = rlang::caller_env())
  OrigGeom <- ggplot2:::check_subclass(orig_geom, "Geom", env = parent.frame(n = 2), call = parent.frame(n = 2))
  expected_aspect_ratio <- expected_aspect_ratio %||% 1
  GeomWithCheck <-
    ggproto(
      paste0(class(OrigGeom)[1], "_with_ggpointdensity_checks"),
      OrigGeom,
      draw_layer = function(self, data, params, layout, coord) {
        grobs <- OrigGeom$draw_layer(data, params, layout, coord)
        is_using_coord_fixed <- inherits(layout$coord, "CoordFixed")
        if(is_using_coord_fixed) return(grobs) # don't add check if actual aspect ratio is known ahead of time
        check_added_once <- FALSE
        grobs <- lapply(grobs, function(grob) {
          if (inherits(grob, "zeroGrob")) { # don't check if nothing is drawn
            return(grob)
          }
          if(!check_added_once) { # add check to at most one grob
            class(grob) <- c("check_aspect_grob", class(grob))
            grob["expected_aspect_ratio"] <- list(expected_aspect_ratio)
            check_added_once <- TRUE
          }
          return(grob)
        })
        grobs
      }
    )
  return(GeomWithCheck)
}

#' @rdname geom_pointdensity
#' @export
stat_pointdensity <- function(mapping = NULL,
                              data = NULL,
                              geom = "point",
                              position = "identity",
                              ...,
                              adjust = 1,
                              aspect.ratio = ggplot2::theme_get()$aspect.ratio,
                              na.rm = FALSE,
                              method = "auto",
                              method.args = list(),
                              show.legend = NA,
                              inherit.aes = TRUE) {
  GeomWithCheck <- addCheckToGeom(geom, expected_aspect_ratio = aspect.ratio)
  layer(
    data = data,
    mapping = mapping,
    stat = StatPointdensity,
    geom = GeomWithCheck,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      adjust = adjust,
      aspect.ratio = aspect.ratio,
      na.rm = na.rm,
      method = method,
      method.args = method.args,
      ...
    )
  )
}

#' @export
#' @noRd
#' @importFrom grid makeContext
#' @method makeContext check_aspect_grob
makeContext.check_aspect_grob <- function(x) {
  # Grab viewport information
  vp <- if (is.null(x$vp)){
    grid::viewport()
  } else{
    x$vp
  }
  width <- grid::convertWidth(unit(1, "npc"), "inch", valueOnly = TRUE)
  height <- grid::convertHeight(unit(1, "npc"), "inch", valueOnly = TRUE)
  actual_aspect_ratio <-  height/width
  expected_aspect_ratio <-  x$expected_aspect_ratio

  if (getOption("ggpointdensity.verbose", default = FALSE)) cli::cli_inform(c(
      "Actual aspect ratio: {actual_aspect_ratio}.",
      "Expected aspect ratio: {expected_aspect_ratio}."
    ))

  if(!isTRUE(all.equal(expected_aspect_ratio, actual_aspect_ratio)) ) cli::cli_warn(c(
    "Actual plot aspect ratio does not match {.arg aspect.ratio} of {.code StatPointdensity}",
    "!" =  "The shown densities are be a bit off.",
    "*" = "Actual plot aspect ratio is {actual_aspect_ratio}.",
    "*" =  "{.arg aspect.ratio} used by {.code StatPointdensity} is {expected_aspect_ratio}.",
    ">" = "Consider using one of the three options below to fix this:",
    "*" = "Add {.code    theme(aspect.ratio = {expected_aspect_ratio}) +}. (This will resize your plot independently of the data).",
    "*" = "Add {.code     coord_fixed(ratio = {expected_aspect_ratio}) +} and set {.code aspect.ratio=NULL}. (This will resize your plot depending on the data)",
    "*" = "Add  {.code aspect.ratio={actual_aspect_ratio}} to {.fun stat_pointdensity}/{.fun geom_pointdensity}. (This will correct the density calculation to match your plot's current size)"
    ), class = "actual_aspect_ratio_does_not_match_expectation")
  NextMethod()
}


#' ggproto class, see [ggplot2::Stat()]
#' @format NULL
#' @usage NULL
#' @export
StatPointdensity <- ggproto("StatPointdensity", Stat,
  default_aes = aes(color = after_stat(density)),
  required_aes = c("x", "y"),

  extra_params = c("aspect.ratio", Stat$extra_params),

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

    data <- ggplot2:::remove_missing(data, params$na.rm,
      c(required_aes, self$non_missing_aes),
      ggplot2:::snake_class(self),
      finite = FALSE # Note that in ggplot2's Stat this is TRUE
    )

    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]

    args <- c(list(data = quote(data), scales = quote(scales)), params)
    ggplot2:::dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      rlang::try_fetch(
        rlang::inject(self$compute_panel(data = data, scales = scales, !!!params, coord = layout$coord)),
        error = function(cnd) {
         cli::cli_warn("Computation failed in {.fn {ggplot2:::snake_class(self)}}", parent = cnd)
          ggplot2:::data_frame0()
        }
      )
    })
  },

  setup_params = function(data, params) {
    if (identical(params$method, "auto")) {
      params$method <- "kde2d"
    }

    params
  },

  compute_group = function(data, scales, adjust = 1, method = "auto",
                           method.args = list(), ..., aspect.ratio = NULL, coord) {
    scale_views <- coord$setup_panel_params(scales$x, scales$y)
    dx <- diff(xrange <- scale_views$x.range)
    dy <- diff(yrange <- scale_views$y.range)

    ratio <- dy/dx

    is_using_aspect.ratio <- !is.null(aspect.ratio)
    is_using_coord_fixed <- inherits(coord, "CoordFixed")

    if (is_using_aspect.ratio && is_using_coord_fixed) cli::cli_abort(c(
      "Only one of {.arg aspect.ratio} and {.fn coord_fixed} must be used",
      ">" = "either set {.code aspect.ratio=NULL} (the default) or remove {.fn coord_fixed}/{.fn coord_equal}."
    ))
    if (is_using_aspect.ratio) {
      ratio <- dy/dx/aspect.ratio
    }
    if (is_using_coord_fixed) {
      ratio <- 1/coord$ratio
    }

    if (identical(method, "default")) {
      # find an appropriate bandwidth (radius), pretty ad-hoc:
      r2 <- (dx*sqrt(ratio) + dy/sqrt(ratio)) / 70 * adjust

      # find an appropriate bandwidth (radius), pretty ad-hoc:                                                                                                                                                                                                                                                                    xrange <- diff(scales$x$get_limits()) * adjust                                                                                                                                                                                                                                                                              yrange <- diff(scales$y$get_limits()) * adjust                                                                                                                                                                                                                                                                              r2 <- (xrange + yrange) / 70                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            # since x and y may be on different scales, we need a                                                                                                                                                                                                                                                                       # factor to weight x and y distances accordingly:                                                                                                                                                                                                                                                                           xy <- xrange / yrange
      data$density <- count_neighbors(data$x, data$y, r2 = r2, xy = 1/ratio)

    } else if (identical(method, "kde2d")) {

      finites <- is.finite(data$x) & is.finite(data$y)
      ddata <- data[finites,]
      base.args <- list(
        x = ddata$x,
        y = ddata$y,
        lims = c(scales$x$dimension(), scales$y$dimension()))
      if (!is.element("n", names(method.args))) {
        method.args["n"] <- 100
      }
      if (!is.element("h", names(method.args))) {
        bandwidth_std <-  c(x=MASS::bandwidth.nrd(ddata$x), y=MASS::bandwidth.nrd(ddata$y))

        #alternative bandwidth calculation based on plot size that tries to match above
        n <- nrow(ddata)+1 # workaround for single-row data set
        bandwidth_limits <- 4 * 1.06 * c(dx, dy) / (2*qnorm(1/n/2, lower.tail = FALSE)) * n^(-1/5)

        bandwidth <- pmax(bandwidth_limits, bandwidth_std, na.rm = TRUE)
        bandwidth <- mean(bandwidth)

        if (getOption("ggpointdensity.verbose", default = FALSE)) cli::cli_inform(c(
          "selected joint bandwidth is {round(bandwidth, 6)}.",
          "i" = "bandwidths derived from effective scale limits are {round(bandwidth_limits, 6)}.",
          "i" = "bandwidths derived from data variance are {round(bandwidth_std, 6)}."
        ))

        bandwidth <- sqrt(c(x=1/ratio , y=ratio)) * bandwidth
        method.args$h <- bandwidth * adjust
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


    data$ndensity <- data$density/max(data$density)

    data
  }
)


#' A cross between a scatter plot and a 2D density plot
#'
#' The pointdensity geom is used to create scatterplots where each point is
#' colored by the number of neighboring points. This is useful to visualize the
#' 2D-distribution of points in case of overplotting.
#'
#'
#' @aliases geom_pointdensity stat_pointdensity StatPointdensity
#' @param mapping Set of aesthetic mappings created by
#' [`aes()`][aes] or [`aes_()`][aes_]. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default
#' mapping at the top level of the plot. You must supply `mapping` if
#' there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If `NULL`, the default, the data is inherited from the plot data as
#' specified in the call to [`ggplot()`][ggplot].
#'
#' A `data.frame`, or other object, will override the plot data. All
#' objects will be fortified to produce a data frame. See
#' [`fortify()`][fortify] for which variables will be created.
#'
#' A `function` will be called with a single argument, the plot data. The
#' return value must be a `data.frame`, and will be used as the layer
#' data. A `function` can be created from a `formula` (e.g. `~
#' head(.x, 10)`).
#' @param stat The statistical transformation to use on the data for this
#' layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param \dots Other arguments passed on to [`layer()`][layer].
#' This includes `adjust`, a multiplicate bandwidth adjustment used to
#' adjust the distance threshold to consider two points as neighbors, i.e. the
#' radius around points in which neighbors are counted. For example,
#' `adjust = 0.5` means use half of the default. Other arguments may be
#' aesthetics, used to set an aesthetic to a fixed value, like `shape =
#' 17` or `size = 3`. They may also be parameters to the paired geom/stat.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#' warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' `NA`, the default, includes if any aesthetics are mapped. `FALSE`
#' never includes, and `TRUE` always includes. It can also be a named
#' logical vector to finely select the aesthetics to display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that
#' define both data and aesthetics and shouldn't inherit behaviour from the
#' default plot specification, e.g. [`borders()`][borders].
#' @author Lukas P.M. Kremer
#' @references https://GitHub.com/LKremer/ggpointdensity
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#' library(ggpointdensity)
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
#'   facet_wrap( ~ group)
#'
#' # ... or point shape and size:
#' dat_subset <- sample_frac(dat, .1)  # smaller data set
#' ggplot(data = dat_subset, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity(size = 3, shape = 17) +
#'   scale_color_viridis()
#'
#' # Zooming into the axis works as well, keep in mind
#' # that xlim() and ylim() change the density since they
#' # remove data points.
#' # It may be better to use `coord_cartesian()` instead.
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity() +
#'   scale_color_viridis() +
#'   xlim(c(-1, 3)) + ylim(c(-5, 15))
#'
#' ggplot(data = dat, mapping = aes(x = x, y = y)) +
#'   geom_pointdensity() +
#'   scale_color_viridis() +
#'   coord_cartesian(xlim = c(-1, 3), ylim = c(-5, 15))
#'
#' @export geom_pointdensity
geom_pointdensity <- function(mapping = NULL,
                              data = NULL,
                              stat = "pointdensity",
                              position = "identity",
                              ...,
                              method = "auto",
                              aspect.ratio = ggplot2::theme_get()$aspect.ratio,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  GeomPointWithCheck <- addCheckToGeom(ggplot2::GeomPoint, expected_aspect_ratio = aspect.ratio)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointWithCheck,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      aspect.ratio = aspect.ratio,
      na.rm = na.rm,
      ...
    )
  )
}
