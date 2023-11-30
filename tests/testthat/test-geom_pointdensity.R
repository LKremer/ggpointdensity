n <- 100
set.seed(1)
df_isotropic_normal <- data.frame(idx = seq_len(n), x = rnorm(n), y = rnorm(n))

test_that("isotropic normal", {
  p1 <- ggplot(df_isotropic_normal, aes(x, y)) + geom_pointdensity(method= "kde2d") + coord_fixed()
  p1
  expect_no_warning(print(p1))
})

test_that("no variation in one axis", {
  p1 <- ggplot(df_isotropic_normal, aes(0*x, y)) + geom_pointdensity(method= "kde2d") + coord_fixed()
  p1
  expect_no_warning(print(p1))
})

test_that("no variation in both axis", {
  p1 <- ggplot(df_isotropic_normal, aes(0*x, 0*y)) + geom_pointdensity(method= "kde2d") + coord_fixed()
  p1
  expect_no_warning(print(p1))
})

test_that("single row data set", {
  p1 <- ggplot(df_isotropic_normal[1, ], aes(x, y)) + geom_pointdensity(method= "kde2d") + coord_fixed()
  expect_no_error(print(p1))
})

test_that("zero row data set", {
  df <- data.frame(x = numeric(0), y = numeric(0))
  p1 <- ggplot(df, aes(x, y)) + geom_pointdensity(method= "kde2d") + coord_fixed()
  expect_no_error(print(p1))
})
