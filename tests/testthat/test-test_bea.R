testthat::test_that("BEA data downloaded correctly", {
  bea_raw <- get_bea()
  all(c("K","D","I") %in% names(bea_raw))
  testthat::expect_contains(class(bea_raw$K),"data.frame")
  # temp <-bea_raw$K[1:3]
  # testthat::expect_equal(temp[1,22], c(33416975, 8628555, 20078458)) #in millions of dollars

  bea_clean <- clean_bea(bea_raw)
  bea <- make_vars_bea(bea_clean)
#   print(bea_raw$K[1:3]$DataValue_2022)

#
#
#   testthat::expect_equal(bea_raw$D[1:3]$DataValue_2022,c(2825242,1161318,539585)) #in millions of dollars
#   testthat::expect_equal(bea_raw$I[1:3]$DataValue_2022,c(3372359, 1308964, 658043)) #in millions of dollars
  testthat::expect_equal(
  bea$data %>%
    dplyr::filter( lubridate::year(date)==2022) %>%
    dplyr::select(
    `Private nonresidential fixed assets.K`,
    Equipment.K,
    Structures.K,
    `Private nonresidential fixed assets.D`,
    Equipment.D,
    Structures.D,
    `Private nonresidential fixed assets.I`,
    Equipment.I,
    Structures.I,
  ) %>%
    as.matrix() %>%
    as.vector(),
  c(
    33416975/10^3, 8628555/10^3, 20078458/10^3, #in billions of dollars
    2825242/10^3,1161318/10^3,539585/10^3, #in billions of dollars
    3372359/10^3, 1308964/10^3, 658043/10^3 #in billions of dollars
  )
)

})


