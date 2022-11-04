# -----------------------------------------------------------------------------
testthat::context("getStateName()")

setup_spatial_data <- function() {

  skip_on_cran()
  skip_on_travis()

  # try to set up spatial data. Skip if fails.
  spatialDataDir <- try(getSpatialDataDir(), silent = TRUE)

  if (!exists('NaturalEarthAdm1')) {
    tryCatch(getSpatialDataDir(),
             error = function(error) {
               setSpatialDataDir("~/Data/Spatial_0.8")
             })
    tryCatch(loadSpatialData("NaturalEarthAdm1"),
             error = function(error) {
               message("Could not load NaturalEarthAdm1")
             })
  }
  if (!exists("NaturalEarthAdm1")) {
    skip("Could not load NaturalEarthAdm1")
  }
  return(spatialDataDir)

}

testthat::test_that("handles errors correctly", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- setup_spatial_data()

  testthat::expect_error(getStateName())
  testthat::expect_error(getStateName(0, 100))
  testthat::expect_error(getStateName(-400, 0))

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
#   spatialDataDir <- setup_spatial_data()
#
#   testthat::expect_match(getStateName(-108, 48), "Montana")
#   testthat::expect_match(getStateName(2.1, 41.38), "Barcelona")
#   testthat::expect_match(getStateName(c(-108, 2.1), c(48, 41.38)), "Montana|Barcelona")
#   testthat::expect_equal(getStateName(1,1), NA_character_)
#
#   # Teardown
#   if (class(spatialDataDir) == "character") {
#     setSpatialDataDir(spatialDataDir)
#   } else {
#     .removeSpatialDataDir()
#   }
#
# })


testthat::test_that("subsetting with countryCodes works", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- setup_spatial_data()

  testthat::expect_match(getStateName(2.1, 41.38, countryCodes = c("ES")), "Barcelona")
  testthat::expect_equal(getStateName(c(2.1, -108), c(41.38, 48), countryCodes = "ES"), c("Barcelona", NA_character_))

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
  spatialDataDir <- setup_spatial_data()

  testthat::expect_s3_class(getStateName(-100, 48, countryCodes = c("US"), allData = TRUE), "data.frame")
  testthat::expect_equal(dim(getStateName(-100, 48, countryCodes = c("US"), allData = TRUE)), c(1, ncol(NaturalEarthAdm1) - 1 )) # SFDF has an extra 'geometry' column
  testthat::expect_s3_class(getStateName(c(-100, -90), c(48, 38), countryCodes = c("US"), allData = TRUE), "data.frame")
  testthat::expect_equal(dim(getStateName(c(-100, -90), c(48, 38), countryCodes = c("US"), allData = TRUE)), c(2, ncol(NaturalEarthAdm1) - 1 )) # SFDF has an extra 'geometry' column

  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    .removeSpatialDataDir()
  }

})

