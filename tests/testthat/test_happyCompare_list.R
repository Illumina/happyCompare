context("happyCompare_list")

test_that("extract.happyCompare_list works", {
  e = extract(happyCompare_list, table = "summary")
  expect_true("happy_summary" %in% class(e))
  expect_true(all(c(".Id", "Group.Id", "Sample.Id", "Replicate.Id") %in% names(e)))
  
  e = extract(happyCompare_list, table = "extended")
  expect_true("happy_extended" %in% class(e))
  expect_true(all(c(".Id", "Group.Id", "Sample.Id", "Replicate.Id") %in% names(e)))
})

