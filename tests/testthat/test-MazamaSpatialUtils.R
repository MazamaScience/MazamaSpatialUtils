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

  
