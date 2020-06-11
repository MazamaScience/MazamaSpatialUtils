# ----- US_stateCodeToName ----------------------------------------------------------------

testthat::context("US_stateCodeToName()")

test_that("Returns expected output", {
  expect_equal(US_stateCodeToName("VT"), "Vermont")
  expect_equal(US_stateCodeToName(c("SD", "NY", "WA", "CA")),
               c("South Dakota", "New York", "Washington", "California"))
})

# ----- US_stateCodeToFIPS ----------------------------------------------------------------

testthat::context("US_stateCodeToFIPS()")

test_that("Returns expected output", {
  expect_equal(US_stateCodeToFIPS("AZ"), "04")
  expect_equal(US_stateCodeToFIPS(c("CO", "CT", "FL", "ID")),
               c("08", "09", "12", "16"))
})

# ----- US_stateFIPSToName ----------------------------------------------------------------

testthat::context("US_stateFIPSToName()")

test_that("Returns expected output", {
  expect_equal(US_stateFIPSToName("17"), "Illinois")
  expect_equal(US_stateFIPSToName(c("55", "48", "41", "01")),
               c("Wisconsin","Texas", "Oregon", "Alabama"))
})

# ----- US_stateFIPSToCode ----------------------------------------------------------------

testthat::context("US_stateFIPSToCode()")

test_that("Returns expected output", {
  expect_equal(US_stateFIPSToCode("17"), "IL")
  expect_equal(US_stateFIPSToCode(c("55", "48", "41", "01")),
               c("WI", "TX", "OR", "AL"))
})

# ----- US_stateNameToCode ----------------------------------------------------------------

testthat::context("US_stateNameToCode()")

test_that("Returns expected output", {
  expect_equal(US_stateNameToCode("Rhode Island"), "RI")
  expect_equal(US_stateNameToCode(c("Indiana", "Kansas", "Maine", "New Hampshire")),
               c("IN", "KS", "ME", "NH"))
})

# ----- US_stateNameToFIPS ----------------------------------------------------------------

testthat::context("US_stateNameToFIPS()")

test_that("Returns expected output", {
  expect_equal(US_stateNameToFIPS("Utah"), "49")
  expect_equal(US_stateNameToFIPS(c("South Carolina", "Puerto Rico", "New Mexico", "Georgia")),
               c("45", "72", "35", "13"))
})
