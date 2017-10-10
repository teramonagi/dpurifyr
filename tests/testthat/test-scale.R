context("scale")

expect_almost_equal <- function(x, y, tolerance=1e-4)
{
  expect_equal(sqrt(sum(x-y)^2), 0.0, tolerance=tolerance)
}

test_that("scale_standarad results agree with base::scale as value", {
  df <- head(iris, 10)
  pp <- dpurifyr::scale_standard(df, Sepal.Width, Petal.Length)
  expect_almost_equal(pp$Sepal.Width,  base::scale(df$Sepal.Width), tolerance=0.001)
  expect_almost_equal(pp$Petal.Length, base::scale(df$Petal.Length), tolerance=0.001)
  expect_equal(nrow(pp), nrow(df))
  expect_equal(ncol(pp), ncol(df))
  expect_named(pp, names(iris))
})

test_that("scale_minmax results agree with its definition", {
  df <- head(iris, 10)
  for(rng in list(c(0, 1), c(-1, 1))){
    pp <- dpurifyr::scale_minmax(df, Sepal.Width, range=rng)
    x <- df$Sepal.Width
    answer <- (x - min(x)) / (max(x) - min(x)) * abs(diff(rng)) + min(rng)
    expect_almost_equal(pp$Sepal.Width,  answer, tolerance=1e-4)
    expect_equal(nrow(pp), nrow(df))
    expect_equal(ncol(pp), ncol(df))
  }
})
