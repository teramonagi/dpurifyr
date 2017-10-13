context("encode")

test_that("encode_label results agree with numerical values assigned in dictionary-ordered", {
  df <- data.frame(x=c('a', 'b', 'a', 'c'), hoge=1:4)
  pp <- dpurifyr::encode_label(df, x)
  expect_equal(pp$x, c(1,2,1,3))
  expect_equal(nrow(pp), nrow(df))
  expect_equal(ncol(pp), ncol(df))
  expect_named(pp, names(df))
})

test_that("encode_count results agree with numerical values of their count", {
  df <- data.frame(x=c('a', 'b', 'a', 'c'), hoge=1:4)
  pp <- dpurifyr::encode_count(df, x)
  expect_equal(pp$x, c(2,1,2,1))
  expect_equal(nrow(pp), nrow(df))
  expect_equal(ncol(pp), ncol(df))
  expect_named(pp, names(df))
})
