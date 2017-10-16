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

test_that("encode_count results agree with one-hot encoding by hand", {
  df <- data.frame(x=c('a', 'b', 'a', 'c'), hoge=1:4)
  pp <- dpurifyr::encode_onehot(df, x)
  expect_equal(pp$x_a, c(1,0,1,0))
  expect_equal(pp$x_b, c(0,1,0,0))
  expect_equal(pp$x_c, c(0,0,0,1))
  expect_equal(nrow(pp), nrow(df))
  expect_equal(ncol(pp), ncol(df)+2)
})

test_that("encode_count using two variables with different separator should be also possible", {
  df <- data.frame(x=c('a', 'b', 'a', 'c'), hoge=c(1,1,2,2))
  pp <- dpurifyr::encode_onehot(df, x, hoge, sep="-")
  expect_equal(pp$`x-a`, c(1,0,1,0))
  expect_equal(pp$`x-b`, c(0,1,0,0))
  expect_equal(pp$`x-c`, c(0,0,0,1))
  expect_equal(dplyr::pull(pp, "hoge-1"), c(1,1,0,0))
  expect_equal(dplyr::pull(pp, "hoge-2"), c(0,0,1,1))
  expect_equal(nrow(pp), nrow(df))
  expect_equal(ncol(pp), ncol(df)+3)
})
