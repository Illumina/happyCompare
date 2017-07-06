context("analysis.R")

samplesheet_path <- system.file("extdata/samplesheets", "pcrfree_vs_nano.tests.csv", package = "happyCompare")
happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE)

test_that("extract_metrics works", {
  
  e <- extract_metrics(happy_compare, table = "summary")
  expect_is(e, "happy_summary")
  expect_true(all(c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix") %in% names(e)))
  expect_equal(dim(e)[1], 16)
  expect_equal(dim(e)[2], 21)
  
  e <- extract_metrics(happy_compare, table = "extended")
  expect_is(e, "happy_extended")
  expect_true(all(c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix") %in% names(e)))
  expect_equal(dim(e)[1], 704)
  expect_equal(dim(e)[2], 69)
  
  e <- extract_metrics(happy_compare, table = "pr.indel.pass")
  expect_is(e, "happy_roc")
  expect_true(all(c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix") %in% names(e)))
  expect_equal(dim(e)[1], 9126)
  expect_equal(dim(e)[2], 70)
  
  expect_error(suppressWarnings(e <- extract_metrics(happy_compare, table = "pr.snp.pass")))
  
})

test_that("estimate_hdi works", {
  
  # prepare data
  d <- extract_metrics(happy_compare, table = "extended") %>% 
    dplyr::filter(Type == "INDEL", Subtype == "D1_5", Filter == "PASS")
  expect_is(d, "data.frame")
  expect_equal(dim(d)[1], 32)
  expect_equal(dim(d)[2], 69)
  
  # estimate hdis with / without per-replicate results
  successes_col <- "TRUTH.TP"
  totals_col <- "TRUTH.TOTAL"
  group_cols <- c("Group.Id", "Subset", "Type", "Subtype")
  expect_warning(e1 <- estimate_hdi(df = d, successes_col = successes_col, totals_col = totals_col, 
                                    group_cols = group_cols, aggregate_only = TRUE, sample_size = 1000))
  expect_warning(e2 <- estimate_hdi(df = d, successes_col = successes_col, totals_col = totals_col, 
                                    group_cols = group_cols, aggregate_only = FALSE, sample_size = 1000))
  
  # check output cols
  required_cols <- c(group_cols, "replicate_id", "successes", "totals", "mu", "sigma", 
                     "alpha0", "beta0", "alpha1", "beta1", "lower", "observed_p", "estimated_p", 
                     "upper", "hdi_range")
  expect_true(all(required_cols %in% colnames(e1)))
  expect_true(all(required_cols %in% colnames(e2)))
  
  # check that an aggregate replicate has been added
  expect_true(".aggregate" %in% e1$replicate_id)
  expect_true(".aggregate" %in% e2$replicate_id)
  
  # check that aggregate_only works as intended
  expect_equal(sum(is.na(e1["lower"])), 24)
  expect_equal(sum(is.na(e2["lower"])), 0)
  
  # check that subsets where TRUTH.TOTAL == 0 have been excluded 
  d_n_subsets <- dim(d)[1]
  d_n_subsets_empty <- sum(d[totals_col] == 0)
  e1_n_subsets <- dim(e1 %>% dplyr::filter(replicate_id != ".aggregate"))[1]
  e2_n_subsets <- dim(e2 %>% dplyr::filter(replicate_id != ".aggregate"))[1]
  expect_equal(d_n_subsets - d_n_subsets_empty, e1_n_subsets)
  expect_equal(d_n_subsets - d_n_subsets_empty, e2_n_subsets)
  
  # see package vignettes for details on method calibration
  
})

test_that(".estimate_hdi works", {
  
  s <- c(2, 2, 2)
  t <- c(2, 2, 2)
  e1 <- .estimate_hdi(successes = s, totals = t)
  e2 <- .estimate_hdi(successes = s, totals = t, aggregate_only = FALSE)
  
  expect_equal(dim(e1)[1], 4)
  expect_equal(dim(e2)[1], 4)
  
  # additional tests handled by estimate_hdi above
  
})
