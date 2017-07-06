## add tests for I/O

context("io.R")

samplesheet_path <- system.file("extdata/samplesheets", "pcrfree_vs_nano.tests.csv", package = "happyCompare")

test_that("read_samplesheet loads data with no errors and returns the expected happy_compare object", {
  
  expect_error(happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE), NA)
  
  expect_is(happy_compare, "happy_compare")
  expect_true(all(names(happy_compare) %in% c("samplesheet", "happy_results", "ids")))
  
  s <- happy_compare$samplesheet
  expect_is(s, "data.frame")
  expect_equal(dim(s)[1], 4)
  expect_equal(dim(s)[2], 4)
  
  r <- happy_compare$happy_results
  expect_is(r, "happy_result_list")
  expect_equal(length(r), 4)
  
  expect_is(r[[1]], "happy_result")
  expect_true(all(names(r[[1]]) %in% c("summary", "extended", "pr_curve")))
  
  e <- happyR::extract_results(r, table = "summary")
  expect_is(e, "data.frame")
  expect_equal(dim(e)[1], 16)
  expect_equal(dim(e)[2], 18)
  expect_true("from" %in% colnames(e))
  
  e <- happyR::extract_results(r, table = "extended")
  expect_is(e, "data.frame")
  expect_equal(dim(e)[1], 704)
  expect_equal(dim(e)[2], 66)
  expect_true("from" %in% colnames(e))
  
  p <- r[[1]]$pr_curve
  expect_is(p, "environment")
  expect_true(all(names(p) %in% c("INDEL_SEL", "INDEL_PASS", "INDEL", "all")))
  
  e <- happyR::extract_results(r, table = "pr.indel.pass")
  expect_is(e, "data.frame")
  expect_equal(dim(e)[1], 9126)
  expect_equal(dim(e)[2], 67)
  expect_true("from" %in% colnames(e))
  
}) 

test_that("read_samplesheet_ returns a happy_compare object", {
  
  samplesheet <- suppressMessages(readr::read_csv(samplesheet_path))[1:2,]
  
  expect_error(read_samplesheet_(samplesheet = samplesheet), NA)
  
  broken_samplesheet <- as.character(samplesheet)
  expect_error(read_samplesheet_(samplesheet = broken_samplesheet))
  
})
