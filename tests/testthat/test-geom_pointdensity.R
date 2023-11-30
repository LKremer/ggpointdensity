n <- 100
set.seed(1)
df_isotropic_normal <- data.frame(idx = seq_len(n), x = rnorm(n), y = rnorm(n))

for( method in c("default", "kde2d") ) {
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
