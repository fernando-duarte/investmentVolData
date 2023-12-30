test_that("Fama-French data downloaded correctly", {
  famafrench_raw <- get_famafrench()
  famafrench <- clean_famafrench(famafrench_raw)

  rf <- famafrench$monthly |>
    dplyr::select(date, rf) |>
    dplyr::collect()
  rf_daily <- famafrench$daily |>
    dplyr::select(date, rf) |>
    dplyr::collect()

  expect_contains(class(famafrench_raw),"list")
  expect_contains(class(famafrench),"list")
  expect_contains(class(famafrench[[1]]),"data.frame")
  expect_contains(class(famafrench[[2]]),"data.frame")
  expect_contains(names(famafrench),c("daily","monthly"))

  expect_equal(famafrench$monthly %>%
                 dplyr::filter( lubridate::ymd("2023-11-01") == date) %>%
                 dplyr::select(`mkt-rf`, smb, hml, rf) %>%
                 as.matrix() %>%
                 as.vector(),
               c(8.84, 0, 1.65, 0.44)
               )
  })
