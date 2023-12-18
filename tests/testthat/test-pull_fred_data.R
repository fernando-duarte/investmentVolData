test_that("Pull FRED returns nonempty data.frame", {
  testkey <- "7fdf94c38c6355269067736a82bf7874"
  df <- pull_fred_data(testkey)
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
  expect_true(dim(df)[1] > 0 & dim(df)[2] > 0)
})
