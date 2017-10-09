context("standardization")

test_that("scale results agree with base::scale as value", {
  df <- head(iris)
  pp <- dpurifyr::scale(df, Sepal.Width, Petal.Length)
  expect_equal(sum(pp$Sepal.Width  - base::scale(df$Sepal.Width ))^2, 0, tolerance=0.001)
  expect_equal(sum(pp$Petal.Length - base::scale(df$Petal.Length))^2, 0, tolerance=0.001)
  expect_length(pp, 5)
})
