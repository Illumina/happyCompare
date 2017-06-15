context("data.R")

test_that("demo happy_compare is as expected", {
  d <- happy_compare
  
  expect_true("happy_compare" %in% class(d))
  expect_equal(length(d), 2)
  expect_true(all(names(d) %in% c("samplesheet", "happy_results")))
  
  s <- d$samplesheet
  expect_equal(dim(s)[1], 3)
  expect_equal(dim(s)[2], 5)
  expect_true(all(names(s) %in% c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix", ".Id")))
  
  h <- d$happy_results
  expect_true(class(h) == "happy_results_list")
  expect_equal(length(h), 3)
  expect_true(all(sapply(h, class) == "happy_result"))
  expect_true(all(sapply(h, function(x) all(names(x) %in% c("summary", "extended", "pr_curve")))))
})

test_that("demo happy_extended is as expected", {
  d <- happy_extended
  
  expect_true("happy_extended" %in% class(d))
  expect_equal(dim(d)[1], 756)
  expect_equal(dim(d)[2], 70)
  
  required_cols <- c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix", ".Id", 
    "TRUTH.TOTAL", "TRUTH.TP", "Subset", "Type", "Subtype")
  expect_true(all(required_cols %in% colnames(d)))
})
