# ----- countryCodeToName ------------------------------------------------------

testthat::context("countryCodeToName()")

test_that("Returns expected output", {
  expect_equal(countryCodeToName("DE"), "Germany")
  expect_equal(countryCodeToName(c("YE", "SV", "ES", "DZ")),
               c("Yemen", "El Salvador", "Spain", "Algeria"))
})

# ----- countryCodeToFIPS ------------------------------------------------------

testthat::context("countryCodeToFIPS()")

test_that("Returns expected output", {
  expect_equal(countryCodeToFIPS("DE"), "GM")
  expect_equal(countryCodeToFIPS(c("YE", "SV", "ES", "DZ")),
               c("YM", "ES", "SP", "AG"))
})

# ----- countryFIPSToName ------------------------------------------------------

testthat::context("countryFIPSToName()")

test_that("Returns expected output", {
  expect_equal(countryFIPSToName("GM"), "Germany")
  expect_equal(countryFIPSToName(c("YM", "ES", "SP", "AG")),
               c("Yemen", "El Salvador", "Spain", "Algeria"))
})

# ----- countryFIPSToCode ------------------------------------------------------

testthat::context("countryFIPSToCode()")

test_that("Returns expected output", {
  expect_equal(countryFIPSToCode("GM"), "DE")
  expect_equal(countryFIPSToCode(c("YM", "ES", "SP", "AG")),
               c("YE", "SV", "ES", "DZ"))
})

# ----- countryNameToCode ------------------------------------------------------

testthat::context("countryNameToCode()")

test_that("Returns expected output", {
  expect_equal(countryNameToCode("Germany"), "DE")
  expect_equal(countryNameToCode(c("Yemen", "El Salvador", "Spain", "Algeria")),
               c("YE", "SV", "ES", "DZ"))
})

# ----- countryNameToFIPS ------------------------------------------------------

testthat::context("countryNameToFIPS()")

test_that("Returns expected output", {
  expect_equal(countryNameToFIPS("Germany"), "GM")
  expect_equal(countryNameToFIPS(c("Yemen", "El Salvador", "Spain", "Algeria")),
               c("YM", "ES", "SP", "AG"))
})
