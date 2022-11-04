# -----------------------------------------------------------------------------
testthat::context("getUSCounty()")

setup_counties <- function() {

  skip_on_cran()
  skip_on_travis()

  spatialDataDir <- try(getSpatialDataDir(), silent = TRUE)

  if (!exists('USCensusCounties')) {
    tryCatch(getSpatialDataDir(),
             error = function(error) {
               setSpatialDataDir("~/Data/Spatial_0.8")
             })
    tryCatch(loadSpatialData("USCensusCounties"),
             error = function(error) {
               message("Could not load USCensusCounties")
             })
  }
  if (!exists("USCensusCounties")) {
    skip("Could not load USCensusCounties")
  }
  return (spatialDataDir)

}

testthat::test_that("handles errors correctly", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- setup_counties()

  testthat::expect_error(getUSCounty())
  testthat::expect_error(getUSCounty(dataset = "USCensusCounties"))
  testthat::expect_error(getUSCounty(0,100))
  testthat::expect_error(getUSCounty(-400, 0))

  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    .removeSpatialDataDir()
  }

})

# NOTE:  Slow!
# testthat::test_that("returns correct name", {
#
#   skip_on_cran()
#   skip_on_travis()
#
#   # Setup
#   spatialDataDir <- setup_counties()
#
#   testthat::expect_match(getUSCounty(-112.97, 35.1), "Yavapai")
#   testthat::expect_match(getUSCounty(-97.5, 38.7), "Saline")
#   testthat::expect_match(getUSCounty(c(-112.97, -97.5), c(35.1, 38.7)), "Yavapai|Saline")
#   testthat::expect_equal(getUSCounty(10,10), NA_character_)
#
#   # Teardown
#   if (class(spatialDataDir) == "character") {
#     setSpatialDataDir(spatialDataDir)
#   } else {
#     .removeSpatialDataDir()
#   }
#
# })

testthat::test_that("subsetting by stateCode works", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- setup_counties()

  testthat::expect_match(getUSCounty(-112.97, 35.1, stateCodes = "AZ"), "Yavapai")
  testthat::expect_match(getUSCounty(-97.5, 38.7, stateCodes = "KS"), "Saline")
  testthat::expect_match(getUSCounty(c(-112.97, -97.5), c(35.1, 38.7), stateCodes = c("AZ", "KS")), "Yavapai|Saline")
  testthat::expect_equal(getUSCounty(c(-112.97, -97.5), c(35.1, 38.7), stateCodes = "AZ"), c("Yavapai", NA_character_))

  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    .removeSpatialDataDir()
  }

})

testthat::test_that("allData returns are correct dimension and type", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- setup_counties()

  testthat::expect_s3_class(getUSCounty(-112.97, 35.1, stateCodes = "AZ", allData = TRUE), "data.frame")
  testthat::expect_equal(dim(getUSCounty(-112.97, 35.1, stateCodes = "AZ", allData = TRUE)), c(1, ncol(USCensusCounties) - 1)) # SFDF has an extra 'geometry' column
  testthat::expect_s3_class(getUSCounty(c(-112.97, -97.5), c(35.1, 38.7), stateCodes = c("AZ", "KS"), allData = TRUE), "data.frame")
  testthat::expect_equal(dim(getUSCounty(c(-112.97, -97.5), c(35.1, 38.7), stateCodes = c("AZ", "KS"), allData = TRUE)), c(2, ncol(USCensusCounties) - 1)) # SFDF has an extra 'geometry' column

  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    .removeSpatialDataDir()
  }

})

