context("chain")

expect_almost_equal <- function(x, y, tolerance=1e-4)
{
  expect_equal(sqrt(sum(x-y)^2), 0.0, tolerance=tolerance)
}

test_that("scale_standarad & scale_minmax combination are agree with each preprocessing and dpurifyr::apply", {
  df <- head(iris, 10)
  rng <- c(0, 1)
  pp <- df %>%
    dpurifyr::scale_standard(Sepal.Width, Petal.Length) %>%
    dpurifyr::scale_minmax(Petal.Width, range=rng)

  # Check each transformation
  expect_almost_equal(pp$Sepal.Width,  base::scale(df$Sepal.Width), tolerance=0.001)
  expect_almost_equal(pp$Petal.Length, base::scale(df$Petal.Length), tolerance=0.001)
  x <- df$Petal.Width
  answer <- (x - min(x)) / (max(x) - min(x)) * abs(diff(rng)) + min(rng)
  expect_almost_equal(pp$Petal.Width,  answer, tolerance=1e-4)

  # dpurifyr::apply should reproduce the same preprocessing result
  expect_true(all(dpurifyr::apply(df, pp) == pp))
})
