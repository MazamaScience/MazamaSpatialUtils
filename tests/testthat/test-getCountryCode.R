# -----------------------------------------------------------------------------
testthat::context("getCountryCode()")

testthat::test_that("handles errors correctly", {
  testthat::expect_error(getCountryCode())
  testthat::expect_error(getCountryCode(0,100))
})

testthat::test_that("returns correct name", {
  testthat::expect_match(getCountryCode(2, 47), "FR")
  testthat::expect_match(getCountryCode(-80, 40), "US")
  testthat::expect_match(getCountryCode(c(120,-17), c(-1.5,15)), "ID|SN")
  testthat::expect_match(getCountryCode(c(-87.1, 73.6, 26.2, -123), c(5.5, -52.9, 35.8, 48.7)),
                         "CR|AU|GR|US")
})

testthat::test_that("subsetting with countryCodes works", {
  testthat::expect_match(getCountryCode(2, 47), "FR")
  testthat::expect_match(getCountryCode(2, 47, countryCodes = c("FR")), "FR")
  testthat::expect_match(getCountryCode(2, 47, countryCodes = "FR"), "FR")
})

testthat::test_that("allData returns are correct dimension and type", {
  testthat::expect_s3_class(getCountryCode(2, 47, allData = TRUE), "data.frame")
  testthat::expect_equal(dim(getCountryCode(2, 47, allData = TRUE)), c(1,3))
  testthat::expect_s3_class(getCountryCode(c(120,-17), c(-1.5,15), allData = TRUE), "data.frame")
  testthat::expect_equal(dim(getCountryCode(c(120,-17), c(-1.5,15), allData = TRUE)), c(2,3))
})



