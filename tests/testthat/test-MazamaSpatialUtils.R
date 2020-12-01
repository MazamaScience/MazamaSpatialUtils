# -----------------------------------------------------------------------------
# Convenience functions

setup_spatial_data <- function() {

  skip_on_cran()
  skip_on_travis()

  # try to set up spatial data. Skip if fails.
  spatialDataDir <- try(getSpatialDataDir(), silent = TRUE)

  if (!exists('NaturalEarthAdm1')) {
    tryCatch(getSpatialDataDir(),
             error = function(error) {
               setSpatialDataDir("~/Data/Spatial")
             })
    tryCatch(loadSpatialData("NaturalEarthAdm1"),
             error = function(error) {
               message("Could not load NaturalEarthAdm1")
             })
  }
  if (!exists("NaturalEarthAdm1")) {
    skip("Could not load NaturalEarthAdm1")
  }
  return (spatialDataDir)

}

# -----------------------------------------------------------------------------
testthat::context("Environment dataDir")


testthat::test_that("setSpatialDataDir and getSpatialDataDir work correctly", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- try(getSpatialDataDir(), silent = TRUE)

  setSpatialDataDir("~")
  testthat::expect_equal(path.expand("~"), getSpatialDataDir())
  setSpatialDataDir(getwd())
  testthat::expect_equal(getwd(), getSpatialDataDir())

  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    removeSpatialDataDir()
  }

})

# -----------------------------------------------------------------------------
context("iso2ToIso3()")

test_that("Input is validated properly", {
  expect_error(iso2ToIso3("USA"), 'countryCodes must be all ISO 3166-1 alpha-2')
  expect_error(iso2ToIso3(c("US","NZL")), 'countryCodes must be all ISO 3166-1 alpha-2')
})

test_that("Returns expected output", {
  expect_match(iso2ToIso3("US"), "USA")
  expect_equal(iso2ToIso3(c("US", "NZ")), c("USA", "NZL"))
})

# -----------------------------------------------------------------------------
context("iso3ToIso2()")

test_that("Input is validated properly", {
  expect_error(iso3ToIso2("US"), 'countryCodes must be all ISO 3166-1 alpha-3')
  expect_error(iso3ToIso2(c("US","NZL")), 'countryCodes must be all ISO 3166-1 alpha-3')
})

test_that("Returns expected output", {
  expect_match(iso3ToIso2("NZL"), "NZ")
  expect_equal(iso3ToIso2(c("USA", "NZL")), c("US", "NZ"))
})

# -----------------------------------------------------------------------------
context("codeToCountry()")

test_that("Returns expectected output", {
  expect_equal(codeToCountry("US"), "United States")
  expect_equal(codeToCountry(c("US", "NZ")), c("United States", "New Zealand"))
})

# -----------------------------------------------------------------------------
context("countryToCode()")

test_that("Returns expected output", {
   expect_equal(countryToCode("Netherlands Antilles"), "AN")
   expect_equal(countryToCode(c("United States", "Canada")), c("US", "CA"))
})

# -----------------------------------------------------------------------------
context("stateToCode()")


test_that("returns expected output", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- setup_spatial_data()

  expect_equal(stateToCode("Washington"), "WA")
  expect_equal(stateToCode("Cantabria"), "CB")

  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    removeSpatialDataDir()
  }

})


# -----------------------------------------------------------------------------
context("codeToState")

test_that("returns expected output", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- setup_spatial_data()

  expect_equal(codeToState("WA", "US"), "Washington")
  expect_equal(codeToState("CB", "ES"), "Cantabria")

  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    removeSpatialDataDir()
  }

})

test_that("warns when there are multiple states for a code", {

  skip_on_cran()
  skip_on_travis()

  # Setup
  spatialDataDir <- setup_spatial_data()

  expect_warning(codeToState("WA"), "9 states with code")
  expect_warning(codeToState("CB"), "15 states with code")

  # Teardown
  if (class(spatialDataDir) == "character") {
    setSpatialDataDir(spatialDataDir)
  } else {
    removeSpatialDataDir()
  }

})

# -----------------------------------------------------------------------------
context("dissolve")

test_that("errors are handled intellegently", {
  expect_error(dissolve(SimpleCountries, field = "missing"), "Field 'missing' not found")
})

# NOTE:  This takes a long time and is only tested before CRAN submission

# test_that("dissolves into correct object", {
#
#   skip_on_cran()
#   skip_on_travis()
#
#   regions <- dissolve(SimpleCountries, field = "UN_region")
#   expect_is(regions, "SpatialPolygonsDataFrame")
#   expect_equal(length(unique(SimpleCountries$UN_region)), length(regions$UN_region))
#
# })

