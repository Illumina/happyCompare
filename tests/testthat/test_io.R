## add tests for I/O

context("io.R")

samplesheet_path <- system.file("tests/data/pcrfree_vs_nano/pcrfree_vs_nano.tests.csv", 
                                package = "happyCompare")

test_that("read_samplesheet loads data with no errors and returns the expected happy_compare object", {
  
  expect_error(happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE), NA)
  
  expect_is(happy_compare, "happy_compare")
  expect_true(all(names(happy_compare) %in% c("samplesheet", "happy_results", "ids", "build_metrics")))
  
  s <- happy_compare$samplesheet
  expect_is(s, "data.frame")
  expect_equal(dim(s)[1], 4)
  expect_equal(dim(s)[2], 5)
  
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
  
  b <- happy_compare$build_metrics
  expect_is(b, "build_metrics_list")
  expect_equal(length(b), 4)
  
  expect_is(b[[1]], "data.frame")
  expect_equal(dim(b[[1]])[1], 1)
  expect_equal(dim(b[[1]])[2], 136)
  
}) 

test_that("read_samplesheet_ returns an error if it receives an invalid samplesheet", {
  
  samplesheet <- suppressMessages(readr::read_csv(samplesheet_path))[1:2,]
  expect_error(read_samplesheet_(samplesheet = samplesheet), NA)
  
  samplesheet <- suppressMessages(readr::read_csv(samplesheet_path))[1:2, 1:4]
  expect_error(read_samplesheet_(samplesheet = samplesheet), NA)  
  
  samplesheet <- suppressMessages(readr::read_csv(samplesheet_path))[1:2, 1:4]
  samplesheet$Custom.column <- c("foo", "bar")
  
  samplesheet <- suppressMessages(readr::read_csv(samplesheet_path))[1:2, 1:2]
  expect_error(read_samplesheet_(samplesheet = samplesheet))
  
  broken_samplesheet <- as.character(suppressMessages(readr::read_csv(samplesheet_path))[1:2,])
  expect_error(read_samplesheet_(samplesheet = broken_samplesheet))
  
})

test_that(".validate_metrics_headers works", {
  
  expect_error(happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE), NA)
  b <- happy_compare$build_metrics
  expect_warning(.validate_metrics_headers(b), NA)
  
  colnames(b[[1]])[1] <- "File.Name"
  expect_warning(.validate_metrics_headers(b))
  expect_warning(conflicts <- .validate_metrics_headers(b))
  expect_equal(length(conflicts), 2)
  
})