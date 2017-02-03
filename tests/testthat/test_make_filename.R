library(testthat)
context("make_filename")

test_that("make_filename with single inputs", {
  expect_equal(make_filename(2013),"accident_2013.csv.bz2")
  expect_equal(make_filename("2013"),"accident_2013.csv.bz2")
})

test_that("make_filename error", {
  throws_error(make_filename("two thousand thirteen"))
})
