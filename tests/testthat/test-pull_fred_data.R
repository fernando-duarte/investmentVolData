test_that("Pull FRED returns nonempty tibble", {
  ff <- get_famafrench()
  expect_equal(class(ff), c("list"))
  expect_equal(class(ff[[1]]$subsets$data[[1]]),c("spec_tbl_df","tbl_df","tbl","data.frame") )
})
