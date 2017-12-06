# Environment dataDir ----------------------------------------------------------

testthat::context("Environment dataDir")

testthat::test_that("setSpatialDataDir and getSpatialDataDir work correctly", {
  setSpatialDataDir("~")
  testthat::expect_equal(path.expand("~"), getSpatialDataDir())
  setSpatialDataDir(getwd())
  testthat::expect_equal(getwd(), getSpatialDataDir())
})

# -----------------------------------------------------------------------------

testthat::context("get~() functions")

testthat::test_that("get functions handle errors correctly", {

  testthat::expect_error(getCountryCode(),
               'argument "lon" is missing, with no default')
  testthat::expect_error(getCountryCode(0,100))

})

testthat::test_that("get functions return correct name", {

  testthat::expect_match(getCountryCode(2, 47), "FR")
  testthat::expect_match(getCountryCode(-80, 40), "US")
  testthat::expect_match(getCountryCode(c(120,-17), c(-1.5,15)), "ID|SN")
  testthat::expect_match(getCountryCode(c(-87.1, 73.6, 26.2, -123), c(5.5, -52.9, 35.8, 48.7)),
                         "CR|HM|GR|US")
  
  testthat::expect_match(getTimezone(2, 47), "Europe/Paris")
  testthat::expect_match(getTimezone(-80, 40), "America/New_York")
  testthat::expect_match(getTimezone(c(120,-7), c(-1.5,15)), "Asia/Makassar|Africa/Bamako")

})

testthat::test_that("subsetting with countryCodes works", {

  testthat::expect_match(getCountryCode(2, 47), "FR")
  testthat::expect_match(getCountryCode(2, 47, countryCodes=c("FR")), "FR")
  testthat::expect_match(getCountryCode(2, 47, countryCodes="FR"), "FR")

  testthat::expect_match(getTimezone(2, 47), "Europe/Paris")
  testthat::expect_match(getTimezone(2, 47, countryCodes=c("FR")), "Europe/Paris")
  testthat::expect_match(getTimezone(2, 47, countryCodes="FR"), "Europe/Paris")

})

testthat::test_that("allData returns are correct dimension and type", {

  testthat::expect_s3_class(getCountryCode(2, 47, allData=TRUE), "data.frame")
  testthat::expect_equal(dim(getCountryCode(2, 47, allData=TRUE)), c(1,6))
  testthat::expect_s3_class(getCountryCode(c(120,-17), c(-1.5,15), allData=TRUE), "data.frame")
  testthat::expect_equal(dim(getCountryCode(c(120,-17), c(-1.5,15), allData=TRUE)), c(2,6))

  testthat::expect_s3_class(getTimezone(2, 47, allData=TRUE), "data.frame")
  testthat::expect_equal(dim(getTimezone(2, 47, allData=TRUE)), c(1,7))
  testthat::expect_s3_class(getTimezone(c(120,-17), c(-1.5,15), allData=TRUE), "data.frame")
  testthat::expect_equal(dim(getTimezone(c(120,-17), c(-1.5,15), allData=TRUE)), c(2,7))

})

testthat::test_that("getPolygonID handles errors correctly", {

  testthat::expect_error(getPolygonID(iris))
  testthat::expect_is(getPolygonID(SimpleTimezones), "character")
  testthat::expect_is(getPolygonID(SimpleCountries), "character")
  
})

# -----------------------------------------------------------------------------

testthat::context("summarizeByPolygon()")

testthat::test_that("summarizeByPolygon properly summarizes", {
  
  testthat::expect_equal(dim(summarizeByPolygon(longitude = c(20.383333, -110, 25.433333, 11.330556, 101.766667, -110, -110),
                                                latitude = c(36.066667, 71, 36.416667, 43.318611, 36.633333, 71, 71),
                                                value = c(80, 43, 29, 55, 12, 32, 23),
                                                SPDF = SimpleCountries, useBuffering = FALSE,
                                                FUN = mean, varName="valueMean")), c(nrow(SimpleCountries@data),2))

})
