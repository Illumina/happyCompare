context("demo data")

test_that("demo data is as expected", {
  d = happyCompare_list

  expect_true(class(d) == "happyCompare_list")
  expect_equal(length(d), 2)
  expect_true(all(names(d) %in% c("samplesheet", "happy_results")))
  
  s = d$samplesheet
  expect_equal(dim(s)[1], 3)
  expect_equal(dim(s)[2], 5)
  expect_true(all(names(s) %in% c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix", ".Id")))
  
  h = d$happy_results
  expect_true(class(h) == "happy_results_list")
  expect_equal(length(h), 3)
  expect_true(all(sapply(h, class) == "happy_result"))
  expect_true(all(sapply(h, function(x) all(names(x) %in% c("summary", "extended", "pr_curve")))))
})