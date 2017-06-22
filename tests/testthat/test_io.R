## add tests for I/O

context("io.R")

samplesheet_path <- system.file("extdata/samplesheets", "pcrfree_vs_nano.csv", package = "happyCompare")

test_that("read_samplesheet loads data with no errors and returns the expected happy_compare object", {
  
  expect_error(happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE), NA)
  
  expect_is(happy_compare, "happy_compare")
  expect_true(all(names(happy_compare) %in% c("samplesheet", "happy_results")))
  
  s <- happy_compare$samplesheet
  expect_true(tibble::is.tibble(s))
  expect_equal(dim(s)[1], 4)
  expect_equal(dim(s)[2], 5)
  
  r <- happy_compare$happy_results
  expect_is(r, "happy_result_list")
  expect_equal(length(r), 4)
  
  expect_is(r[[1]], "happy_result")
  expect_true(all(names(r[[1]]) %in% c("summary", "extended", "pr_curve")))
  
  s <- r[[1]]$summary
  expect_is(s, "tibble")
  expect_equal(dim(s)[1], 4)
  expect_equal(dim(s)[2], 17)
  
  e <- r[[1]]$extended
  expect_is(e, "tibble")
  expect_equal(dim(e)[1], 176)
  expect_equal(dim(e)[2], 65)
  
  p <- r[[1]]$pr_curve
  expect_is(p, "environment")
  expect_true(all(names(p) %in% c("INDEL_SEL", "INDEL_PASS", "INDEL", "SNP_SEL", "SNP_PASS", "SNP", "all")))
  expect_true(tibble::is.tibble(p$SNP_PASS))
  expect_equal(dim(p$SNP_PASS)[1], 1442)
  expect_equal(dim(p$SNP_PASS)[2], 66)
  
}) 

test_that("read_samplesheet_ returns a happy_compare object", {
  
  samplesheet <- suppressMessages(readr::read_csv(samplesheet_path))[1:2,]
  happy_results <- c(happyR::read_happy(happy_prefix = samplesheet$happy_prefix[1], lazy = TRUE),
                     happyR::read_happy(happy_prefix = samplesheet$happy_prefix[2], lazy = TRUE))
  ids <- samplesheet$Replicate.Id
  expect_error(read_samplesheet_(samplesheet = samplesheet, happy_results = happy_results, ids = ids), NA)
  
  broken_samplesheet <- as.data.frame(samplesheet)
  expect_error(read_samplesheet_(samplesheet = broken_samplesheet, happy_results = happy_results, ids = ids))
  
  broken_happy_results <- happy_results[[1]]
  expect_error(read_samplesheet_(samplesheet = samplesheet, happy_results = broken_happy_results, ids = ids))
  
  broken_ids <- as.list(ids)
  expect_error(read_samplesheet_(samplesheet = samplesheet, happy_results = happy_results, ids = broken_ids))
  
})
