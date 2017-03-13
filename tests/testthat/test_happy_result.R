context("happy_result")

test_that("demo data is as expected", {
  hrl = happy_result_list
  expect_true(class(hrl)[1] == "happy_result_list")
  expect_equal(length(hrl), 2)
  expect_true(all(sapply(hrl, class) == "happy_result"))
  
  expect_true(class(hapdata)[1] == "happy_result")
  roc = hapdata$pr_curve$all
  expect_true(class(roc)[1] == "happy_roc")
  expect_equal(dim(roc)[1], 62091)
  expect_equal(dim(roc)[2], 65)
})