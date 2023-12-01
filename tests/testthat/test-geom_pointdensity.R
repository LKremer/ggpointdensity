set.seed(1)
n <- 100
df_isotropic_normal <- data.frame(idx = seq_len(n), x = rnorm(n), y = rnorm(n))

n <- 1000
df_three_isotropic_normals <- data.frame(idx = seq_len(n*3), grp = rep(1:3, n), x = rnorm(n) + rep(c(1,1,1.5), n)*10, y = rnorm(n)+rep(c(1,2,2),n)*10)

for( method in c("default", "kde2d") ) {
  test_that(cli::format_inline("coord_fixed(method=\"{method}\") runs without warning ({.code method=\"{method}\"})"), {
    p1 <- ggplot(df_isotropic_normal, aes(x, y)) + geom_pointdensity(method= method)  + coord_fixed()
    expect_no_warning(print(p1))
  })

  test_that(cli::format_inline("aspect.ratio runs without warning ({.code method=\"{method}\"})"), {
    a.r <- 3
    p1 <- ggplot(df_isotropic_normal, aes(x, y)) + geom_pointdensity(method= method, aspect.ratio = a.r) + theme(aspect.ratio = a.r)
    expect_no_warning(print(p1))
  })

  test_that(cli::format_inline("aspect.ratio adjusts density for three isotropic normals ({.code method=\"{method}\"})"), {

    a.r <- 10
    p1 <-
      ggplot(df_three_isotropic_normals, aes(x, y, label = grp, group =1)) + stat_pointdensity(method= method, size=5, geom="text", aspect.ratio = a.r, adjust=500)  + theme(aspect.ratio = a.r)
    p1
    plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
    get_mean_label_idx <- function(idx) mean(plot_data$density[plot_data$label==idx])
    expect_lt(get_mean_label_idx(1), get_mean_label_idx(2))
    expect_gt(get_mean_label_idx(2), get_mean_label_idx(3))
    expect_lt(get_mean_label_idx(1), get_mean_label_idx(3))

    a.r <- 1
    p1 <-
      ggplot(df_three_isotropic_normals, aes(x, y, label = grp)) + stat_pointdensity(method= method, size=5, geom="text",  aspect.ratio = a.r, adjust=70) + theme(aspect.ratio = a.r)
    p1
    plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
    get_mean_label_idx <- function(idx) mean(plot_data$ndensity[plot_data$label==idx])
    expect_gt(get_mean_label_idx(2), get_mean_label_idx(3))
    expect_equal(get_mean_label_idx(1),  get_mean_label_idx(3), tolerance = 0.2)
  })

  if(method != "default") {
  test_that(cli::format_inline("aspect.ratio adjusts density ({.code method=\"{method}\"})"), {
    n <- 1
    df <- data.frame(idx = 1:3, x = rep(c(1,1,1.1), n), y = rep(c(1,2,2), n))

    a.r <- 10
    p1 <-
      ggplot(df, aes(x, y, label = idx)) + stat_pointdensity(method= method, size=20, geom="text", aspect.ratio = a.r)  + theme(aspect.ratio = a.r)
    p1
    plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
    expect_lt(plot_data$ndensity[1], plot_data$ndensity[2])
    expect_gt(plot_data$ndensity[2], plot_data$ndensity[3])
    expect_lt(plot_data$ndensity[1], plot_data$ndensity[3])

    a.r <- 1
    p1 <-
      ggplot(df, aes(x, y)) + geom_pointdensity(method= method, size=20, aspect.ratio = a.r) + theme(aspect.ratio = a.r)
    p1
    plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
    expect_gt(plot_data$ndensity[2], plot_data$ndensity[3])
    expect_equal(plot_data$ndensity[1],  plot_data$ndensity[3])
  })

  test_that(cli::format_inline("coord_fixed adjusts density ({.code method=\"{method}\"})"), {
    n<-1
    df <- data.frame(idx = 1:3, x = rep(c(1,1,1.1), n), y = rep(c(1,2,2), n))
    p1 <-
      ggplot(df, aes(x, y, label =idx)) + stat_pointdensity(method= method, size=20, geom="text")  + coord_fixed()
    plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
    expect_lt(plot_data$ndensity[1], plot_data$ndensity[2])
    expect_gt(plot_data$ndensity[2], plot_data$ndensity[3])

    p1 <-
      ggplot(df, aes(x, y)) + geom_pointdensity(method= method, size=20)  + coord_fixed(ratio = 1/10)
    plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
    expect_equal(plot_data$ndensity[1],  plot_data$ndensity[3])
  })
  }

  test_that(cli::format_inline("isotropic normal ({.code method=\"{method}\"}) "), {
    p1 <- ggplot(df_isotropic_normal, aes(x, y)) + geom_pointdensity(method= method) + coord_fixed()
    p1
    expect_no_warning(print(p1))
  })

  test_that(cli::format_inline("no variation in one axis ({.code method=\"{method}\"})"), {
    p1 <- ggplot(df_isotropic_normal, aes(0*x, y)) + geom_pointdensity(method= method) + coord_fixed()
    p1
    expect_no_warning(print(p1))
  })

  test_that(cli::format_inline("no variation in both axis ({.code method=\"{method}\"})"), {
    p1 <- ggplot(df_isotropic_normal, aes(0*x, 0*y)) + geom_pointdensity(method= method) + coord_fixed()
    p1
    expect_no_warning(print(p1))
  })

  test_that(cli::format_inline("single row data set ({.code method=\"{method}\"})"), {
    p1 <- ggplot(df_isotropic_normal[1, ], aes(x, y)) + geom_pointdensity(method= method) + coord_fixed()
    expect_no_error(print(p1))
  })

  test_that(cli::format_inline("zero row data set ({.code method=\"{method}\"})"), {
    df <- data.frame(x = numeric(0), y = numeric(0))
    p1 <- ggplot(df, aes(x, y)) + geom_pointdensity(method= method) + coord_fixed()
    expect_no_error(print(p1))
  })
}
