# -----------------------------------------------------------------------------
testthat::context("internalData()")

testthat::test_that("dplyr functions work", {
  testthat::expect_no_error({
    IS <- SimpleCountries %>% dplyr::filter(countryCode == "IS")
  })
  testthat::expect_no_error({
    IS <- SimpleCountriesEEZ %>% dplyr::filter(countryCode == "IS")
  })
  testthat::expect_no_error({
    IS <- SimpleTimezones %>% dplyr::filter(countryCode == "IS")
  })
})

