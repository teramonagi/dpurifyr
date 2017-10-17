context("split")

test_that("split_date works", {
  df <- data.frame(x=0:5, d=lubridate::ymd("2015-06-29")+0:5, y=0:5)
  pp <- dpurifyr::split_date(df, d)
  expect_equal(pp$x, df$x)
  expect_equal(pp$y, df$y)
  expect_equal(pp$d_year, rep(2015, 6))
  expect_equal(pp$d_month, c(rep(6, 2), rep(7, 4)))
  expect_equal(pp$d_day, c(29:30, 1:4))
  expect_equal(pp$d_weekday, 2:7)
  expect_equal(nrow(pp), nrow(df))
  expect_equal(ncol(pp), ncol(df)+3)
})

test_that("split_date with different separators works", {
  df <- data.frame(x=0:5, d=lubridate::ymd("2015-06-29")+0:5, y=0:5)
  pp <- dpurifyr::split_date(df, d, sep="@")
  expect_equal(pp$x, df$x)
  expect_equal(pp$y, df$y)
  expect_equal(pp$`d@year`, rep(2015, 6))
  expect_equal(pp$`d@month`, c(rep(6, 2), rep(7, 4)))
  expect_equal(pp$`d@day`, c(29:30, 1:4))
  expect_equal(pp$`d@weekday`, 2:7)
  expect_equal(nrow(pp), nrow(df))
  expect_equal(ncol(pp), ncol(df)+3)
})


test_that("split_datetime works", {
  df <- data.frame(
    x=1:5,
    d=lubridate::ymd_hms(c("2013-01-02 01:05:05",
            "2014-03-05 03:19:13",
            "2015-05-17 08:28:28",
            "2016-09-24 15:45:34",
            "2017-11-30 23:55:59"
      ), tz="Asia/Tokyo"),
    y=1:5
  )
  pp <- dpurifyr::split_datetime(df, d)
  expect_equal(pp$x, df$x)
  expect_equal(pp$y, df$y)
  expect_equal(pp$d_year, 2013:2017)
  expect_equal(pp$d_month, c(1,3,5,9,11))
  expect_equal(pp$d_day, c(2,5,17,24,30))
  expect_equal(pp$d_hour, c(1,3,8,15,23))
  expect_equal(pp$d_minute, c(5,19,28,45,55))
  expect_equal(pp$d_second, c(5,13,28,34,59))
  expect_equal(pp$d_timezone, rep("Asia/Tokyo", 5))
  expect_equal(nrow(pp), nrow(df))
  expect_equal(ncol(pp), ncol(df)+7)
})
