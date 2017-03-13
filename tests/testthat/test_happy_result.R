context("happy_result_list")

test_that("demo data is as expected", {
  hrl = happy_result_list
  expect_true(class(hrl)[1] == "happy_result_list")
  expect_equal(length(hrl), 2)
  expect_true(all(sapply(hrl, class) == "happy_result"))
})