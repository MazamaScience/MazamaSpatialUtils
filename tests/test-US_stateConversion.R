# TESTING: http://r-pkgs.had.co.nz/tests.html
#   Vital to package development
#   Part of workflow, makes sure it works
#   Automating tests (aka unit testing)
#   Fewer bugs, better code structure, easier restarts, robust code
#   R is different from other languages -- functional than OO

# Create a file that tests when check runs R CMD check 
# vector of 10-20 known names/ codes/ FIPS gets properly converted into known alternatives
# works for territories, not just states
# handles non-matching names/ codes/ FIPS by returning na

library(testthat)
library(MazamaSpatialUtils)

setSpatialDataDir('~/Data/Spatial')

# ?US_stateCodes
# ?US_stateConversion

# US_stateCodeToName US_stateCodeToFIPS
# US_stateFIPSToName US_stateFIPSToCode
# US_stateNameToCode US_stateNameToFIPS
# US_stateFIPSToCode

#?test_check
#devtools::uses_testthat() 

testthat::context("US_stateConversion()")


# Check if stateCode is 2 letters. 
# Check if stateFIPS is 2 digits.

# Try to set up spatial data. Skip if fails.  
spatialDataDir <- try(getSpatialDataDir(), silent = TRUE)

# ----- US_stateCodeToName ----------------------------------------------------------------

testthat::context("US_stateCodeToName()")

test_that("Returns expected output", {
  expect_equal(US_stateCodeToName("VT"), "Vermont")
  expect_equal(US_stateCodeToName(c("SD", "NY", "WA", "CA")),c("South Dakota", "New York", "Washington", "California"))
})

# ----- US_stateCodeToFIPS ----------------------------------------------------------------

testthat::context("US_stateCodeToFIPS()")

test_that("Returns expected output", {
  expect_equal(US_stateCodeToFIPS("AZ"), "04")
  expect_equal(US_stateCodeToFIPS(c("CO", "CT", "FL", "ID")),c("08", "09", "12", "16"))
})

# ----- US_stateFIPSToName ----------------------------------------------------------------

testthat::context("US_stateFIPSToName()")

test_that("Returns expected output", {
  expect_equal(US_stateFIPSToName("17"), "Illinois")
  expect_equal(US_stateFIPSToName(c("79", "48", "41", "01")),c("Wake Island", "Texas", "Oregon", "Alabama"))
})

# ----- US_stateFIPSToCode ----------------------------------------------------------------

testthat::context("US_stateFIPSToCode()")

test_that("Returns expected output", {
  expect_equal(US_stateFIPSToCode("17"), "Illinois")
  expect_equal(US_stateFIPSToCode(c("79", "48", "41", "01")),c("Wake Island", "Texas", "Oregon", "Alabama"))
})

# ----- US_stateNameToCode ----------------------------------------------------------------

testthat::context("US_stateNameToCode()")

test_that("Returns expected output", {
  expect_equal(US_stateNameToCode("Rhode Island"), "RI")
  expect_equal(US_stateNameToCode(c("Indiana", "Kansas", "Maine", "New Hampshire")),c("IN", "KS", "ME", "NH"))
})

# ----- US_stateNameToFIPS ----------------------------------------------------------------

testthat::context("US_stateNameToFIPS()")

test_that("Returns expected output", {
  expect_equal(US_stateNameToFIPS("Utah"), "49")
  expect_equal(US_stateNameToFIPS(c("South Carolina", "Puerto Rico", "New Mexico", "Georgia")),c("45", "72", "35", "13"))
})

# ----- US_stateFIPSToCode ----------------------------------------------------------------

# Duplicates to HI (Hawaii and Howland Island and Michigan and Midway Islands)
testthat::context("US_stateFIPSToCode()")

test_that("Returns expected output", {
  expect_equal(US_stateFIPSToCode("36"), "NY")
  expect_equal(US_stateFIPSToCode(c("32", "30", "26", "02")),c("NV", "MT", "MI", "AK"))
})