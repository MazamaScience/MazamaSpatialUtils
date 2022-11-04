# -----------------------------------------------------------------------------
context("summarizeByPolygon()")

testthat::test_that("summarizeByPolygon properly summarizes", {

  df <- summarizeByPolygon(
    longitude = c(20.383333, -110, 25.433333, 11.330556, 101.766667, -110, -110),
    latitude = c(36.066667, 71, 36.416667, 43.318611, 36.633333, 71, 71),
    value = c(80, 43, 29, 55, 12, 32, 23),
    SFDF = SimpleCountries,
    useBuffering = FALSE,
    FUN = mean,
    varName = "valueMean"
  )

  CA_polygonID <-
    SimpleCountries %>%
    dplyr::filter(countryCode == "CA") %>%
    dplyr::pull("polygonID")

  CA_valueMean <-
    df %>%
    dplyr::filter(polygonID == CA_polygonID) %>%
    sf::st_drop_geometry() %>%
    dplyr::pull("valueMean")

  testthat::expect_equal(
    CA_valueMean,
    mean(c(43, 32, 23))
  )

})
