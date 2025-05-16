# These tests were written by @jan-glx, big thanks!

set.seed(1)
n <- 100
df <- data.frame(idx = seq_len(n), x = rnorm(n), y = rnorm(n))

test_that("coord_fixed adjusts density correctly (method='kde2d')", {
  n <- 1
  df <- data.frame(
    idx = 1:3,
    x = rep(c(1, 1, 1.1), n),
    y = rep(c(1, 2, 2), n)
  )

  p1 <-
    ggplot(df, aes(x, y, label = idx)) +
    stat_pointdensity(method = "kde2d", size = 20, geom = "text") +
    coord_fixed()
  plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
  expect_lt(plot_data$ndensity[1], plot_data$ndensity[2])
  expect_gt(plot_data$ndensity[2], plot_data$ndensity[3])

  p1 <-
    ggplot(df, aes(x, y)) +
    geom_pointdensity(method = "kde2d", size = 20) +
    coord_fixed(ratio = 1 / 10)
  p1
  plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
  expect_equal(plot_data$ndensity[1], plot_data$ndensity[3])

  p1 <-
    ggplot(df, aes(x * 3, y)) +
    geom_pointdensity(method = "kde2d", size = 20) +
    coord_fixed(ratio = 3 / 10)
  p1
  plot_data <- ggplot2::ggplot_build(p1)$data[[1]]
  expect_lt(plot_data$ndensity[1], plot_data$ndensity[2])
  expect_equal(plot_data$ndensity[1], plot_data$ndensity[3])
})


for (method in c("neighbors", "kde2d")) {
  test_that(
    paste0(
      "coord_fixed(method='",
      method,
      "') runs without warning"
    ),
    {
      p1 <- ggplot(df, aes(x, y)) +
        geom_pointdensity(method = method) +
        coord_fixed()
      expect_no_warning(print(p1))
    }
  )

  test_that(
    paste0("isotropic normal does not warn (method='", method, "')"),
    {
      p1 <- ggplot(df, aes(x, y)) +
        geom_pointdensity(method = method) +
        coord_fixed()
      p1
      expect_no_warning(print(p1))
    }
  )

  test_that(
    paste0(
      "no variation in one axis does not warn (method='",
      method,
      "')"
    ),
    {
      p1 <- ggplot(df, aes(0 * x, y)) +
        geom_pointdensity(method = method) +
        coord_fixed()
      p1
      expect_no_warning(print(p1))
    }
  )

  test_that(
    paste0(
      "no variation in both axis does not warn (method='",
      method,
      "')"
    ),
    {
      p1 <- ggplot(df, aes(0 * x, 0 * y)) +
        geom_pointdensity(method = method) +
        coord_fixed()
      p1
      expect_no_warning(print(p1))
    }
  )

  test_that(
    paste0("single row data set does not error (method='", method, "')"),
    {
      p1 <- ggplot(df[1, ], aes(x, y)) +
        geom_pointdensity(method = method) +
        coord_fixed()
      expect_no_error(print(p1))
    }
  )

  test_that(
    paste0("zero row data set does not error (method='", method, "')"),
    {
      df <- data.frame(x = numeric(0), y = numeric(0))
      p1 <- ggplot(df, aes(x, y)) +
        geom_pointdensity(method = method) +
        coord_fixed()
      expect_no_error(print(p1))
    }
  )
}
