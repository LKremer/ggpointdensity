# ggpointdensity
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ggpointdensity)](https://cran.r-project.org/package=ggpointdensity) 
[![Downloads](https://cranlogs.r-pkg.org/badges/last-month/ggpointdensity?color=brightgreen)](https://cran.r-project.org/package=ggpointdensity)
Introduces `geom_pointdensity()`: A cross between a scatter plot and a 2D density plot.

<img src="img/pointdensity_logo.png" width="60%" />


## Installation
To install the package, type this command in R:
``` r
install.packages("ggpointdensity")

# Alternatively, you can install the latest
# development version from GitHub:
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("LKremer/ggpointdensity")
```

## Motivation
There are several ways to visualize data points on a 2D coordinate system:
If you have lots of data points on top of each other, `geom_point()` fails to
give you an estimate of how many points are overlapping.
`geom_density2d()` and `geom_bin2d()` solve this issue, but they make it impossible
to investigate individual outlier points, which may be of interest.

<img src="img/scatter_dens_bin2d.png" width="100%" />

`geom_pointdensity()` aims to solve this problem by combining the best of both
worlds: individual points are colored by the number of neighboring points.
This allows you to see the overall distribution, as well as individual points.

<img src="img/pointdensity.png" width="50%" />

## Demo
Generate some toy data and visualize it with `geom_pointdensity()`:
``` r
library(ggplot2)
library(dplyr)
library(viridis)
library(ggpointdensity)

dat <- bind_rows(
  tibble(x = rnorm(7000, sd = 1),
         y = rnorm(7000, sd = 10),
         group = "foo"),
  tibble(x = rnorm(3000, mean = 1, sd = .5),
         y = rnorm(3000, mean = 7, sd = 5),
         group = "bar"))

ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_pointdensity() +
  scale_color_viridis()
```

<img src="img/pointdensity.png" width="50%" />

Each point is colored according to the number of neighboring points.
The distance threshold to consider two points as neighbors (smoothing
bandwidth) can be adjusted with the `adjust` argument, where `adjust = 0.5`
means use half of the default bandwidth.
``` r
ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_pointdensity(adjust = .1) +
  scale_color_viridis()
 
ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_pointdensity(adjust = 4) +
  scale_color_viridis()
```
<img src="img/pointdensity_adj.png" width="100%" />

Of course you can combine the geom with standard `ggplot2` features
such as facets...

``` r
# Facetting by group
ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_pointdensity() +
  scale_color_viridis() +
  facet_wrap( ~ group)
```

<img src="img/pointdensity_facet.png" width="75%" />

... or point shape and size:
``` r
dat_subset <- sample_frac(dat, .1)  # smaller data set
ggplot(data = dat_subset, mapping = aes(x = x, y = y)) +
  geom_pointdensity(size = 3, shape = 17) +
  scale_color_viridis()
```

<img src="img/pointdensity_shape.png" width="50%" />

Zooming into the axis works as well, keep in mind that `xlim()` and
`ylim()` change the density since they remove data points.
It may be better to use `coord_cartesian()` instead.

``` r
ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_pointdensity() +
  scale_color_viridis() +
  xlim(c(-1, 3)) + ylim(c(-5, 15))

ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_pointdensity() +
  scale_color_viridis() +
  coord_cartesian(xlim = c(-1, 3), ylim = c(-5, 15))
```

<img src="img/pointdensity_zoom.png" width="100%" />

## Authors
Lukas Kremer and Simon Anders (2019)
